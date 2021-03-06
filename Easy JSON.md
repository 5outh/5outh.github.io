---
title: Haskell Bits 5 - Easily working with JSON
author: Benjamin Kovach
date: 2017-05-11
tags:
  - Haskell
  - Essays 
---

JSON is ubiquitous nowadays, perhaps most importantly for web APIs. We'll probably need to interact with (or build) one of those at some point,
so we must be able to handle JSON in Haskell, right?

Yep - also it's pretty easy. Let's talk about it! First, some boilerplate to get out of the way:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Control.Lens ((^.), (^?))
import Control.Lens.TH
import Data.Aeson.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import GHC.Generics
```

Try not to get too overwhelmed with that giant chunk of imports and extensions. Most of it is only for the
template haskell we'll be using later. The important bit for now is `Data.Aeson` from the `aeson` package, 
which allows us to seamlessly work with JSON. We'll need `lens` and `lens-aeson` packages later on.

Let's pretend we have some API we're building and want to generate a static blob of JSON for the front page.
How about this:

```haskell
index :: Value
index = object
    [ "message" .= String "Congrats!"
    , "status" .= String "YOU_GOT_HERE_SO_OBVIOUSLY_SUCCESSFUL"
    , "metadata" .= object [
        "version" .= Number 9
      ]
    ]
```

`Value` is the type of JSON values in Haskell. We can build up an object using the `object` functions, and a mapping
from keys to `Value`s, generated by `String`, `Number`, and other functions.

We can encode a `Value` with `encode`. It produces a lazy `ByteString`:

```haskell
BL.putStrLn $ encode index
-- { "status":"YOU_GOT_HERE_SO_OBVIOUSLY_SUCCESSFUL","metadata":{"version":9},"message":"Congrats!"}
```

Next up, I'd like to show how a client might interact with this thing. If they have the unpacked `Value`, they can access
the value at the `message` key like this:

```haskell
index ^. key "message" . _String
-- "Congrats!"
```

What's even better is that we don't even need to unpack the `Value`! We can operate directly on the encoded JSON:

```haskell
encode index ^. key "message" . _String
-- "Congrats!"
```

We can dig into the nested fields safely using `^?`:

```haskell
index ^? key "metadata" . key "app_version" . _Number
-- Just 9.0
```

All `Lens` idioms apply. It's easy to get, set, or modify arbitrary fields of JSON objects this way.

Let's say we want to start building up our application and require more type safety. `aeson` makes it easy to
generate encoding and decoding mechanisms for your data types. For instance, we can define the following two
types, generate lenses (via `makeFields`) and `ToJSON` and `FromJSON` instances using a bit of template haskell:

```haskell
data Metadata = Metadata
    { _metadataAppVersion :: Int
    } deriving (Show, Eq, Generic)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_metadata")
        } ''Metadata)
$(makeFields ''Metadata)

data IndexResponse = IndexResponse
    { _indexResponseMessage :: T.Text
    , _indexResponseStatus :: T.Text
    , _indexResponseMetadata :: Metadata
    } deriving (Show, Eq, Generic)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_indexResponse")
        } ''IndexResponse)
$(makeFields ''IndexResponse)
```

Here's the same `index` structure, now typed more explicitly:

```haskell
indexResponse :: IndexResponse
indexResponse = IndexResponse
    "Congrats!"
    "YOU_GOT_HERE_SO_OBVIOUSLY_SUCCESSFUL"
    (Metadata 9)
```

```haskell
BL.putStrLn $ encode indexResponse
-- {"message":"Congrats!","status":"YOU_GOT_HERE_SO_OBVIOUSLY_SUCCESSFUL","metadata":{"app_version":9}}
```

Note that the `appVersion` field gets automatically converted from `camelCase` to `snake_case` with the `camelTo2` option from
`Data.Aeson.Types`. Handy!

We can check that encoding and decoding works, and use more type-safe lenses (in this case, `message`, which was
generated by `makeFields`):

```haskell
(decode (encode indexResponse) :: Maybe IndexResponse) ^. _Just . message
-- "Congrats!"
```

As you can see, dealing with JSON in Haskell is a breeze! What other tips and tricks do you use when dealing
with JSON (de)serialization (in Haskell or otherwise)?

Until next time,

Ben

You can read more about `aeson` and `lens-aeson` in the docs:

- [aeson](https://www.stackage.org/lts-8.13/package/aeson-1.0.2.1)
- [lens-aeson](https://www.stackage.org/lts-8.13/package/lens-aeson-1.0.1)
