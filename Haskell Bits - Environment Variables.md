---
title: Haskell Bits 4 - Environment Variables
date: 2017-02-22
author: Benjamin Kovach
tags:
  - Haskell
  - Essays
---

It's likely that you'll have to deal with environment variables at some point. What I'll describe here is a kicking-off point for
robust environment handling with little overhead. We'll build a tiny library
you can drop into any application that will make dealing
with environment variables for configuration a lot easier. Then I'll show some example usage.

This is all built on top of `System.Environment`, which isn't super nice to use in its raw form.
In particular, there no implicit facilities for type coercion, 
fallback values, or composability. We'll address those problems here.

You'll need the following libraries to get run the code in this post: `transformers`, `split` and `safe`.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Environment hiding (getEnv)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Safe
import Data.List.Split

newtype Env a = Env{ unEnv :: MaybeT IO a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , Alternative
        , MonadPlus
        )

runEnv :: Env a -> IO (Maybe a)
runEnv = runMaybeT . unEnv

-- Lift a `Maybe` into the `Env` context.
liftMaybe :: Maybe a -> Env a
liftMaybe = Env . MaybeT . pure

-- Get an environment variable in its
-- raw form.
getEnv :: String -> Env String
getEnv key =
    liftIO (lookupEnv key) >>= liftMaybe

-- Pull an environment variable from
-- the environment, using a parsing
-- function for conversion.
env :: (String -> Maybe a) -> String -> Env a
env f key = liftMaybe . f =<< getEnv key

-- Pull an optional value from the
-- environment.
optional
    :: (String -> Maybe a)
    -> String
    -> Env (Maybe a)
optional f key =
    (f <$> getEnv key) <|> pure Nothing 

-- Exploit the `Read` interface for a type
-- to read an environment variable.
readEnv :: Read a => String -> Env a
readEnv = env readMay
```

This code was adapted from 
[a comment on reddit](https://www.reddit.com/r/haskell/comments/3bckm7/envy_an_environmentally_friendly_way_to_deal_with/csl3nqa/) (credit to u/Tekmo).

I think this mini-library is "good enough" for a lot of applications. One major drawback is that it doesn't
report missing or improperly formatted environment variables -
 functionality can be added in a relatively straightforward way, however, with
a `MonadThrow` constraint.
This is the simplest thing that does the job well, though, so we'll run with it.

For my example application, I want to be able to pull configuration information
from a set of environment variables.

We can use our mini-library to do this:

```haskell
data Stage 
  = Testing
  | Development
  | Staging 
  | Production
    deriving (Show, Read)

data Version = Version Int Int Int
    deriving Show

data MyEnvironment = MyEnvironment
    { stage :: Stage
    , identifier :: Maybe String
    , version :: Version
    } deriving (Show)

-- Parse a semantic version string like v1.3.3
parseVersion :: String -> Maybe Version
parseVersion versionString =
    case splitOn "." semver of
         [major, minor, patch] ->
             Version
                <$> readMay major
                <*> readMay minor
                <*> readMay patch
         _ -> Nothing
    where semver = tail versionString

-- An environment reader for `MyEnvironment`
myEnv :: Env MyEnvironment
myEnv = MyEnvironment
    <$> (readEnv "APP_STAGE" <|> pure Production)
    <*> optional Just "APP_ID"
    <*> env parseVersion "APP_VERSION"

main :: IO ()
main = runEnv myEnv >>= print
```

Running this as an executable `my_app`, we get the following output (formatting mine):

```haskell
$ APP_STAGE=Testing APP_VERSION=v1.1.1 APP_ID=its_me_mario my_app

Just (
  MyEnvironment
    { stage = Testing
    , identifier = Just "its_me_mario"
    , version = Version 1 1 1
    }
  )
```

Or, with some missing/incomplete information:

```haskell
$ APP_STAGE=nonexistent APP_VERSION=v1.1.4 my_app

Just (
  MyEnvironment
    { stage = Production
    , identifier = Nothing
    , version = Version 1 1 4
    }
  )
```

Ben
