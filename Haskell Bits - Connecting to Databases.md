---
author: Benjamin Kovach
date: 2017-02-08
title: Haskell Bits 3 - Connecting to Databases 
tags:
  - Haskell
  - Essays
---

Today I want to talk about getting Haskell programs to talk to databases.

We'll set out to build something pretty simple: A single table, filled with records of board games. I want to keep
the focus of the post on connecting to various database types, so our board game record will be extremely simple. Here's an
example of the table we'll be working with:

  <table class="checkered full-width">
   <thead class="left">
   <tr>
   <th>id</th>
   <th>name</th>
   <th>designer</th>
   <th>year</th>
   </tr>
   <thead>
  
   <tbody>
   <tr>
   <td>1</td>
   <td>Lords of Waterdeep</td>
   <td>Peter Lee</td>
   <td>2012</td>
   </tr>

   <tr>
   <td>3</td>
   <td>Agricola</td>
   <td>Uwe Rosenberg</td>
   <td>2007</td>
   </tr>

   <tr>
   <td>4</td>
   <td>Race for the Galaxy</td>
   <td>Thomas Lehmann</td>
   <td>2007</td>
   </tr>
   </tbody>

   </table>

Important note: I originally had "Chess" in here as a board game with no known year, but realized after writing
the post and all of the associated code that I don't know who designed it, either! The `designer` should technically
be nullable as well, but it is not in the schema I've defined. Keep that in mind as you read through the post!

I'm going to walk through inserting a single element into a table of the aforementioned schema, but
it will support all of the records above. Try inserting them as an exercise!

We're going to talk about connecting to three different databases:

- sqlite
- postgresql
- mysql

...with two different "flavors" of libary. First, the lower-level "-simple" strain of database libraries, and later,
the higher-level `persistent` library.

### Connecting to `sqlite` with `sqlite-simple`

We need a database:

```sql
$ sqlite3 board_games.db "CREATE TABLE IF NOT EXISTS board_games (id INTEGER PRIMARY KEY,
name TEXT NOT NULL, designer TEXT NOT NULL, year INTEGER);"
```

```haskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Monad.Reader
import Data.Text
import Database.SQLite.Simple
import Data.Maybe (fromJust)

data BoardGame = BoardGame
    { name :: Text
    , designer :: Text
    , year :: Maybe Int
    } deriving (Show, Eq)

instance FromRow BoardGame where
    fromRow = BoardGame <$> field <*> field <*> field

instance ToRow BoardGame where
    toRow BoardGame{..} = toRow (name, designer, year)

createGame :: BoardGame -> ReaderT Connection IO (Int, BoardGame)
createGame game = ask >>= \conn -> do
    liftIO $ execute
        conn 
        "INSERT INTO board_games (name, designer, year) VALUES (?,?,?)"
        game
    boardGameId <- fromIntegral <$> liftIO (lastInsertRowId conn)
    game' <- fromJust <$> readGame boardGameId
    pure (boardGameId, game')

readGame :: Int -> ReaderT Connection IO (Maybe BoardGame)
readGame boardGameId = ask >>= \conn -> do
    games <- liftIO $ query 
        conn
        "SELECT name, designer, year FROM board_games WHERE id = ?"
        (Only boardGameId)
    pure $ case games of
         [g] -> Just g
         _ -> Nothing

main :: IO ()
main = do
    conn <- open "board_games.db"
    flip runReaderT conn $ do
        result <- createGame $ BoardGame
            "Cosmic Encounter"
            "Bill Eberle"
            (Just 2008)
        liftIO $ print result
```

(packages needed: `sqlite-simple`, `mtl`, `text`)

This is the core logic we'll be implementing with each library. We'll just be modifying
this piecemeal for the rest of the implementations. It's the longest code sample in the
post, so don't run off!

Let's walk through this a little, just once. Here's what we're doing:

- Creating a connection (called `conn`)
- Storing it in a read-only environment with `ReaderT` computations
- Creating a new record for Cosmic Encounter
- Then printing it, along with its id in the database.

In order to do this, we have to write a little bit of boilerplate. First is the model definition
for `BoardGame`, and instances of `ToRow` and `FromRow`, which allow us to serialize and deserialize
  from the `sqlite` representation of a `BoardGame`. We also have to write the actual SQL commands;
  not a whole lot is abstracted away from us.

(I lied a little - the model definition is not strictly necessary but it's typically good to
pull data into your program's domain, so I suggest doing this step.)

### Connecting to `postgresql` with `postgresql-simple`

We'll need a database, again (note the syntax is slightly different):

```sql
CREATE TABLE IF NOT EXISTS board_games (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  designer TEXT NOT NULL,
  year INTEGER
);
```

Otherwise, we don't have a ton to change. We only have to touch a few things:

- The way that we procure a `Connection` is slightly different, because we're no longer using a flat file
- `lastInsertRowId` is not a primitive. Postgres supports `RETURNING` syntax so we can get the `id` when we
insert.
- The imports have to change.

That's actually...it. Here are the imports we need:

```haskell
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
```

Here is the new `createGame`:

```haskell
createGame :: BoardGame -> ReaderT Connection IO (Int, BoardGame)
createGame game = ask >>= \conn -> do
    [Only boardGameId] <- liftIO $ query
        conn 
        "INSERT INTO board_games (name, designer, year) VALUES (?,?,?) RETURNING id"
        game
    game' <- fromJust <$> readGame boardGameId
    pure (boardGameId, game')
```

And here is the new connection procuring mechanism (this uses a [postgres connection string](https://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING), so suit it to your needs):

```haskell
-- Inside `main`:
conn <- connectPostgreSQL "host=localhost port=5432 connect_timeout=10"
```

(packages needed: `postgresql-simple`, `mtl`, `text`)

[Check out the full source here](https://gist.github.com/5outh/31acec58bbd91413d71a0df2638fe899).

### Connecting to `mysql` with `mysql-simple`

The last database we'll get this running on is `mysql`. `mysql-simple` was the original
"-simple" library for database management. However, it's also the most different.

First, creating the table:

```sql
CREATE TABLE `board_games` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(64) NOT NULL DEFAULT '',
  `designer` varchar(64) NOT NULL DEFAULT '',
  `year` int(4) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

We'll need the language extension `BangPatterns` (not necessary, but recommended):

```haskell
{-# LANGUAGE BangPatterns #-}
```

and some updated imports:

```haskell
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.Result
```

Our `BoardGame` type can stay the same, but `ToRow` and `FromRow` get replaced with the
typeclasses `QueryParams` and `QueryResults` respectively. Their interfaces are
a little different too:

```haskell
instance QueryParams BoardGame where
    renderParams BoardGame{..} = [render name, render designer, render year]

instance QueryResults BoardGame where
    convertResults [fa,fb,fc] [va,vb,vc] = BoardGame a b c 
        where !a = convert fa va
              !b = convert fb vb
              !c = convert fc vc
    convertResults fs vs  = convertError fs vs 3
```

We need to go back to selecting the last insert id, but there's no primitive for
that, so we inline it and make some small modifications to `createGame`:

```haskell
createGame :: BoardGame -> ReaderT Connection IO (Int, BoardGame)
createGame game = ask >>= \conn -> do
    liftIO $ execute
        conn 
        "INSERT INTO board_games (name, designer, year) VALUES (?,?,?)"
        game
    [Only boardGameId] <- liftIO $ query_ conn "SELECT LAST_INSERT_ID()"
    game' <- fromJust <$> readGame boardGameId 
    pure (boardGameId, game')
```

Connection info is provided using `ConnectInfo` instead of a postgres connection string:

```haskell
connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
    { connectDatabase = "board_games"
    }
```

The last step is to swap the connection line in `main` to:

```haskell
-- Inside main
conn <- connect connectInfo 
```

(packages needed: `postgresql-simple`, `mtl`, `text`)

[Check out the full source here](https://gist.github.com/5outh/5b53643979fb510ded470f8c0bb449e3)

These libraries are not all that different - they're all inspired by one-another.
You may find documentation or tutorials that use one of these libraries and need to use another;
I hope this helps translate between the languages of the three.

Next, I'd like to talk about `persistent`. `persistent` is a higher-level, more fully featured
set of database tooling. It's a lot more "magical" than the "-simple" libraries, but removes the
necessity of some of the boilerplate and inlining of raw SQL that comes with the "-simple" variants.
It's also backend-agnostic which makes for a uniform interface.

### Connecting to `sqlite` with `persistent`

We'll need a data type definition in `persistent`'s template haskell DSL, which we'll put in a module called `Types`:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BoardGame sql=board_games
    name String
    designer String
    year Int Maybe

    UniqueName name

    deriving Show
|]
```

The translated source:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Types

import Database.Persist.Sqlite
import Control.Monad.Logger
import Control.Monad.IO.Class

printIO :: (MonadIO m, Show a) => a -> m ()
printIO = liftIO . print

createGame :: MonadIO m => BoardGame -> SqlPersistT m (Entity BoardGame) 
createGame = insertEntity

readGame :: MonadIO m => Int -> SqlPersistT m (Maybe BoardGame) 
readGame = get . toSqlKey . fromIntegral

main :: IO ()
main = 
    runStdoutLoggingT
        . withSqlitePool "board_games.db" 3
        . runSqlPool
        $ do
            result <- createGame $ BoardGame
                "Cosmic Encounter"
                "Bill Eberle"
                (Just 2008)
            liftIO $ print result

            -- This is the easiest way to re-read a record:
            get (entityKey result) >>= printIO 

            -- Getting by id
            readGame 1 >>= printIO 
            
            -- Get by name too:
            getBy (UniqueName "Cosmic Encounter") >>= printIO 
```

(packages needed: `persistent`, `persistent-template`, `persistent-sqlite`, `mtl`, `monad-logger`)

A couple of things: 

- `SqlPersistT` can be thought of as a "SQL Statement Context" - you can write arbitrary queries
in these blocks. *Each of these is run in its own transaction by default, so be careful! Any exceptions
will roll back any changes.* (thanks to /u/ephrion for
bringing up this point!)
- `BoardGame` and `UniqueName` are types generated by the template haskell in the `Types` module.
- This uses a connection pool with 3 open connections. You can also create a single connection
with `withSqliteConn` instead of `withSqlitePool`.
- `Entity` is a type consisting of a `Key` and a model (in our case, `BoardGame`) - this is analogous
to `(Int, BoardGame)` with the `-simple` libraries.
- `runStdoutLoggingT` prints debug SQL statements to stdout. It can be replaced with `runNoLoggingT`
or `runStderrLoggingT` to modify this behavior.

This is just the tip of the iceberg. See 
[the Yesod book's chapter on persistent](http://www.yesodweb.com/book/persistent),
the [persistent documentation](https://www.stackage.org/lts-8.0/package/persistent-2.6)
and specifically the 
[Database.Persist.Class module](https://www.stackage.org/haddock/lts-8.0/persistent-2.6/Database-Persist-Class.html)
for more information.

Also see the [persistent-sqlite documentation](https://www.stackage.org/package/persistent-sqlite).

### Connecting to `postgres` with `persistent`

Change the `Database.Persist.Sqlite` import to `Database.Persist.Postgres`.

The only other thing to change in order to connect to `postgres` instead is the second line
of `main`, to:

```haskell
. withPostgresqlPool "host=localhost port=5432 connect_timeout=10" 3
```

(packages needed: `persistent`, `persistent-template`, `persistent-postgresql`, `mtl`, `monad-logger`)

Everything else works the same!

[View the full source here](https://gist.github.com/5outh/d2f6a8762701928ceb967d25710c7ed1)

And the [persistent-postgresql documentation](https://www.stackage.org/lts-8.0/package/persistent-postgresql-2.6)

### Connecting to `mysql` with `persistent`

Connecting to mysql is almost as simple. Change the import to `Database.Persist.MySQL`, add the
`ConnectInfo` definition:

```haskell
connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
    { connectDatabase = "board_games"
    }
```

and change the same line in `main` to:

```haskell
. withMySQLPool connectInfo 3
```

(packages needed: `persistent`, `persistent-template`, `persistent-mysql`, `mtl`, `monad-logger`)

Again, everything else works the same.

[View the full source here](https://gist.github.com/5outh/b9d80d7fc174ae5aa87818dfd5158362)

And the [persistent-mysql documentation](https://www.stackage.org/lts-8.0/package/persistent-mysql-2.6)

I've never attempted to run a database-backed haskell application on Windows, so I must apologize
for not showing off how to connect to SQL Server. AFAIK, the only current package that supports this is
 [HDBC](https://github.com/hdbc/hdbc), which I've not used.

What is your preferred way of interacting with databases in Haskell? Which database do you think is the most
pleasant to work with? What parts of this post would you like to see expanded on in the future?
Let me know in the comments!

Ben

