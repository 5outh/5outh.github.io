---
title: Haskell Bits 2 - Application Beginnings 
date: 2017-02-03
author: Benjamin Kovach
tags:
  - Haskell
  - Essays
---

Are you comfortable creating
data types, manipulating lists, composing functions, etc, but not sure how to make a "useful program" 
with haskell?
This is a very common stumbling block when learning. It might be the complaint I've heard the most. 

In this _Haskell Bit_, I want to walk through a pattern I have commonly seen in haskell applications.
The pattern isn't specific to haskell - it's commonplace in tons of programming environments.
It's just a little less obvious how to get here with Haskell. 

Here's the pattern:

- Read some configuration (we'll read it from the environment)
- Set up some program state that will be manipulated over the course of the program
- Run the program! 

The [`mtl`](https://hackage.haskell.org/package/mtl) package will need to be installed to run these examples.

In haskell, we have to be explicit about our the shape of our state and environment. A
common way to represent a program with access to these basic needs, and not much else,
is with the following data type:

```haskell
import Control.Monad.Reader
import Control.Monad.State

type Program state config a = StateT state (ReaderT config IO) a
```

This data type expresses the following:

- We can read and manipulate the internal state of our program (of type `state`)
- We can read (and only read) values of type `config` in our program
- We can access IO

For those who aren't familiar, this is called a "monad transformer stack".
It's just an expression of the effects our program can have.

With this, we're going to build something extremely contrived. It should demonstrate the utility of this
pattern, however. Here's what we'll do:

- Read two environment variables, `COUNT_BY`, and `COUNT_UP_TO`
- It will start at `0`, and count by `COUNT_BY` steps, up to `COUNT_UP_TO`, printing out each value.

To do this, we need IO (to print), a read-only environment (to store the environment variables in), and
some state (the current count). Sounds like it fits the pattern. Let's see what it looks like!

```haskell
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Monoid
import System.Environment

type Program state config a = StateT state (ReaderT config IO) a

-- Run a `Program` with a given state and config, returning
-- a final value and the final state of the `Program`
runProgram :: Program s c a -> s -> c -> IO (a, s)
runProgram p s c = runReaderT (runStateT p s) c

data CounterState = CounterState { currentCount :: Integer }

data CounterConfig = CounterConfig
    { countBy :: Integer
    , countUpTo :: Integer
    }

-- A more specific type for our Counter program
type Counter a = Program CounterState CounterConfig a

-- The initial state we're starting with
initialState :: CounterState
initialState = CounterState 0

-- Some code to read from our environment variables.
-- Note: This is unsafe, and if either environment variable is
-- a) not set, or
-- b) not formatted like an integer,
-- the program will currently error out.
getConfig :: IO CounterConfig
getConfig = do
    countBy' <- read <$> getEnv "COUNT_BY"  
    countUpTo' <- read <$> getEnv "COUNT_UP_TO"
    pure $ CounterConfig countBy' countUpTo' 

-- Our actual program ("business logic")
counter :: Counter () 
counter = do
    count <- gets currentCount
    countUpTo' <- lift $ asks countUpTo

    unless (count > countUpTo') $ do
        liftIO . putStrLn $ "Current count: " <> show count 
        countBy' <- lift $ asks countBy
        let newCount = count + countBy'
        modify (\st -> st{ currentCount = newCount })
        counter

main :: IO ()
main = do
    config <- getConfig
    void $ runProgram counter initialState config
```

To run this, assuming it's compiled to a program called `counter`:

```bash
$ COUNT_BY=13 COUNT_UP_TO=100 counter
Current count: 0
Current count: 13
Current count: 26
Current count: 39
Current count: 52
Current count: 65
Current count: 78
Current count: 91
```

Not so bad! This is totally enough to get started with some more complex programs.
Anything below this point is just polish. I will go into all of these polishing modifications
in more detail in later posts.

Anyway, the following things are a little ugly right now, in my opinion:

- Multiple calls to `lift` in `counter`
- The `modify` call in `counter` is not a cute line of code
- We're confined to `IO` as a base monad
- As mentioned in comments, the configuration code will error out. Better to handle specific cases.

The first thing I want to do is abolish the `lift` calls in `counter`.

We'll need to add the `ConstraintKinds` and `FlexibleContexts` extensions to get this to compile, but here's an updated
program:

```haskell
-- Add this to the top of the file:
{-# LANGUAGE ConstraintKinds#-}
{-# LANGUAGE FlexibleContexts#-}
--

-- An interface that describes the effects our program can have.
type MonadCounter m = 
    ( MonadState CounterState m
    , MonadReader CounterConfig m
    , MonadIO m
    )

counter :: MonadCounter m => m () 
counter = do
    count <- gets currentCount
    countUpTo' <- asks countUpTo

    unless (count > countUpTo') $ do
        liftIO . putStrLn $ "Current count: " <> show count 
        countBy' <- asks countBy
        let newCount = count + countBy'
        modify (\st -> st{ currentCount = newCount })
        counter
```

Everything else can stay the same.
We've begun programming against the `MonadCounter` interface, which `Counter` just happens to satisfy,
so we can still use our `runProgram` function.
The interface contains the `State`/`Reader` functions, but removes the need for `lift`, which is nice.

> Note: If we remove the `MonadIO` constraint in `MonadCounter`,
we're no longer bound to using `IO` as our program's base monad.
It's necessary for our current program (since `liftIO` is called), but for others it may not be.

Next, let's handle the ugly call to `modify`. We can clean this up with lenses.
I'll cover these in greater detail in a later post.
For now, we'll need a separate module, `Types`, containing the following (we also need the [`lens`](https://hackage.haskell.org/package/lens) library):

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data CounterState = CounterState
    { _currentCount :: Integer
    }

data CounterConfig = CounterConfig
    { _countBy :: Integer
    , _countUpTo :: Integer
    }

$(makeLenses ''CounterState)
$(makeLenses ''CounterConfig)
```

This will give us lenses for `currentCount`, `countBy`, and `countUpTo`. We can then write `counter` as:

```haskell
counter :: MonadCounter m => m () 
counter = do
    count <- use currentCount
    countUpTo' <- view countUpTo

    unless (count > countUpTo') $ do
        liftIO . putStrLn $ "Current count: " <> show count 
        countBy' <- view countBy
        currentCount += countBy'
        counter
```

I think that's quite nice and readable.

I'm going to postpone talking about reading environment variables because I want to dedicate a whole _Haskell Bit_ to
reading configuration safely, and this one is getting somewhat long.

Ben
