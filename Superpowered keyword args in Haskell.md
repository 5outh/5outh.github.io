---
date: 2020-09-22T10:30
title: Superpowered keyword args in Haskell
tags:
  - Haskell
  - Essays
---

## Overview

As programs get larger and more complex, the components therein also grow. This
happens on several levels; we often have to deal with growing applications,
packages, modules, and test suites. This blog post is about how to deal with the
growth of the humble function.

If you've ever worked on a production codebase, you know that _one_ function
that lives in a module that several others depend on. It sits at the very core
of your application, growing in complexity to support the ever-expanding needs
of the business. It is the entrypoint into a sea of complexity...and it has
several positional arguments that mean nothing without documentation and other
context. It is a powerful and useful beast, but it must be tamed.

## Domain

Maybe you work on an information system for posts of some kind where a common
operation is fetching a set of posts. Various filters/sorts need to be applied
conditionally based on whatever the caller needs:

- Filter - Posted by a certain person
- Filter - Posted before or after a certain date
- Filter - Liked by a particular other user
- Filter - Commented on by a particular other user
- Filter - Accessible to a particular user
- Filter - Public only
- Sort - Popular posts (top n posts ordered by likes)
- Sort - Most controversial posts (top n posts ordered by number of comments)
- Sort - Most linked posts (linked to by other posts)

## A function

The type signature for the function that does this type of fetch could look
something like this:

```haskell
newtype UserId = UserId Integer
data TimeSpan = TimeSpan { start :: UTCTime, end :: UTCTime }
data Sort = Popular | Controversial | MostLinked

fetchPosts
  :: MonadFetch m
  => Maybe UserId
  -> Maybe TimeSpan
  -> Maybe UserId
  -> Maybe UserId
  -> Maybe UserId
  -> Bool
  -> Sort
  -> m [Post]
fetchPosts userId timeSpan likedBy commentedOnBy accessibleBy publicOnly sort
  = -- omitted
```

This works, but is very error prone. Even though we're making use of newtype
wrappers, the positional arguments could easily get mixed up, causing, for
instance, a fetch for posts `likedBy` a user instead of the expected posts
`commentedOnBy` by a user. Every time a call to this function is made, you have
to be careful to provide the correct arguments in the correct order.

## A safer approach

One way to improve this situation is to make extensive use of `newtype`
wrappers and avoid [[Boolean Blindness]] by utilizing more specific [[Sum Type]]s. This
is a pretty common idiom, and it helps a bit:

```haskell
data VisibilityFilter = PublicOnly | PublicOrPrivate
newtype AuthorId = AuthorId UserId -- and a bunch more, you get the picture

fetchPosts
  :: MonadFetch m
  => Maybe AuthorId
  -> Maybe TimeSpan
  -> Maybe LikedBy
  -> Maybe CommentedOnBy
  -> Maybe AccessibleBy
  -> VisibilityFilter
  -> Sort
  -> m [Post]
fetchPosts userId timeSpan likedBy commentedOnBy accessibleBy publicOnly sort
  = -- ...
```

This is safer, but arguably more annoying to work with due to all the manual
packing of newtypes that needs to be done, and every call is pretty noisy.
Consider what fetching all public posts for a given author would look like:

```haskell
main = do
  posts <- runFetch $
    fetchPosts
      (Just (AuthorId (UserId 1)))
      Nothing
      Nothing
      Nothing
      Nothing
      PublicOnly
      SortPopular
  -- ... do something with posts
```

That is a lot of typing "I don't care" for one function call, and, although we
have moved them from runtime bugs to compile time errors, argument transposition
is still annoying problem that we have to deal with somehow.

Additionally, every time we add a new filter to this function, we need to update
all of the places it is called. This is a very general function that can be used
in many different ways, so that could be a significant number of callsites
depending on the sixe of the project.

## The [[Keyword Args]] Pattern

The pattern I will describe here solves the argument transposition issue and
reduces specifying "I don't care" for very general function call. The trick, if
you can call it that, is to stuff all of those parameters into a data type, then
make that data type defaultable and allow overriding its fields to deal with
special cases like filters and sorting. It reminds me of [[Keyword Args]] in
languages like Python, so I'll refer to this as the _Keyword Args Pattern_.

```haskell
data FetchPostsArgs =
  { authorId :: Maybe UserId
  , timeSpan :: Maybe TimeSpan
  , likedBy :: Maybe UserId
  , commentedOnBy :: Maybe UserId
  , accessibleBy :: Maybe UserId
  , visibilityFilter :: VisibilityFilter
  , sort :: Sort
  }

defaultFetchPostsArgs :: FetchPostsArgs
defaultFetchPostsArgs = FetchPostsArgs
  { authorId = Nothing
  , timeSpan = Nothing
  , likedBy = Nothing
  , commentedOnBy = Nothing
  , accessibleBy = Nothing
  , visibilityFilter = Nothing
  , sort = SortPopular
  }

fetchPosts :: MonadFetch m => FetchPostsArgs -> m [Post]
fetchPosts FetchPostsArgs{..} = -- ...
```

now fetching all public posts for a given author looks like this:

```haskell
main = do
  posts <- runFetch $
    fetchPosts $ defaultFetchPostsArgs { authorId = UserId 1 }
  -- ... do something with posts
```

not only is that easier to write, but it also lets us explicitly specify what we
want to change about the general functionality ("fetch all posts") without
needing to explicitly specify what we don't care about changing (all other
filters and sort methods)

worth noting that `RecordWildCards` is utilized here to retain exactly the same
argument names as before, so the function body would not need to change.

## Adding functionality

suppose we now want to not only filter by a single user, but we also want to
search for posts that contain certain tags. We could modify `FetchPostsArgs` to
support this case:

```haskell
data FetchPostsArgs =
  { authorId :: Maybe UserId
  , timeSpan :: Maybe TimeSpan
  , likedBy :: Maybe UserId
  , commentedOnBy :: Maybe UserId
  , accessibleBy :: Maybe UserId
  , visibilityFilter :: VisibilityFilter
  , sort :: Sort
  , tags :: [Tag]
  }

defaultFetchPostsArgs :: FetchPostsArgs
defaultFetchPostsArgs = FetchPostsArgs
  { authorId = Nothing
  , timeSpan = Nothing
  , likedBy = Nothing
  , commentedOnBy = Nothing
  , accessibleBy = Nothing
  , visibilityFilter = Nothing
  , sort = SortPopular
  , tags = []
  }
```

and update the body of `fetchPosts` as needed.

## Utilizing Booleans and [[Smart Constructor]]s

`fetchPostArgs` looks a lot like a [[Monoid]], and we can certainly frame it
that way, with a little bit of finagling:

```haskell
data Visibility = Public | Private | Protected

data FetchPostsArgs =
  { authorId :: Last UserId
  , timeSpan :: Last TimeSpan
  , likedBy :: Last UserId
  , commentedOnBy :: Last UserId
  , accessibleBy :: Last UserId
  , forbiddenVisibilities :: [Visibility]
  , sorts :: [Sort]
  , tags :: [Tag]
  }

defaultFetchPostsArgs :: FetchPostsArgs
defaultFetchPostsArgs = FetchPostsArgs
  { authorId = Last Nothing
  , timeSpan = Last Nothing
  , likedBy = Last Nothing
  , commentedOnBy = Last Nothing
  , accessibleBy = Last Nothing
  , forbiddenVisibilities = []
  , sorts = []
  , tags = []
  }

instance Semigroup FetchPostsArgs where
  (<>) = -- ... pairwise <>
instance Monoid FetchPostsArgs where
  mempty = -- ... every field is `mempty`
```

with this formulation, we can build up [[Smart Constructor]]s and compose them to
create more specific queries. For example, suppose we want to find **all public
posts by User 2 tagged "Haskell" and "Code" posted less than 7 days ago, that I
(User 1) liked**. Some [[Smart Constructor]]s can help here:

```haskell
publicOnlyArgs :: FetchPostsArgs
publicOnlyArgs = mempty { forbiddenVisibilities = [Private, Protected] }

likedByArgs :: UserId -> FetchPostsArgs
likedByArgs userId = mempty { likedBy = Last (Just userId) }

authoredByArgs :: UserId -> FetchPostsArgs
authoredByArgs userid = mempty { authorId = Last (Just userId) }

taggedArgs :: Tag -> FetchPostsArgs
taggedArgs tag = mempty { tags = [tag] }

postedAfter :: UTCTime -> IO FetchPostsArgs
postedAfter time = do
  now <- getCurrentTime
  pure $ mempty { timeSpan = Last (TimeSpan time now) }
```

and our fetch looks like:

```haskell
main = do
  sevenDaysAgo <- subtractDays 7 <$> getCurrentTime
  timeArgs <- postedAfter sevenDaysAgo

  posts <- runFetch $
    fetchPosts
      $  timeArgs
      <> publicOnlyArgs
      <> authoredByArgs (UserId 2)
      <> likedByArgs (UserId 1)
      <> taggedArgs (Tag "Haskell")
      <> taggedArgs (Tag "Code")

  -- ... do something with posts
```

Using [[Monoid]]s gives us better composition of common operations in many
cases. I might argue that taking this step is unnecessary for this particular
example, but it is at least helpful for illustrative purposes and might come in
handy depending on your use case.

### A small note

Suppose we were writing a function `fetchPostsForUser` instead, which always
required a `UserId` to fetch posts for. In this case, it does make sense to
pull that individual argument outside of the `Args` data type and provide it as
a required parameter, e.g.:

```haskell
fetchPostsForUser :: MonadFetch m => UserId -> FetchPostsArgs -> m [Post]
```

The conceptual reason for this is that `fetchPostsForUser` has no default
behavior without a `UserId`, and we need default behavior for the [[Monoid]] to
remain sensible.

## In Short

If you are dealing with functions that:

- Have many positional arguments
- Are called in a variety of ways to accomplish specific goals
- Have a well-behaved "default" operation, where the arguments simply specify
  refinemesnts on that default operation

then the [[Keyword Args]] pattern might work for you.
