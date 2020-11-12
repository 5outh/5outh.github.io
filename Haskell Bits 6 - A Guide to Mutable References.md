---
title: Haskell Bits 6 - A Guide to Mutable References
author: Benjamin Kovach
date: 2017-06-22
tags:
  - Haskell
  - Essays
---

There are quite a few ways to store mutable data in Haskell. Let's talk about some of
them! Specifically, we will focus on mutable containers that store a single
value that can be modified by one or more threads at any given time.

I'm not going to go into a ton of detail here - I just want to give an overview.
I have provided links to the documentation and other resources at the end of
each section for further reading.

## IORef

``` {=html}
<img src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/HaskellRefs/ioref.png" height="400" style="display:block;margin:auto"></img>
```

First up is `IORef`, the simplest of all containers. It is a sectioned off bit
of mutable memory for any number of threads to read/modify willy-nilly.

We can read this diagram as follows:

- The whole action takes place in the `IO` monad/context.
- A new `IORef` was created in `IO` somewhere and provided to two threads: `t1` and `t2`.
- At some point, `t1` writes a value to the `IORef` using `writeIORef :: IORef a -> a -> IO a`
- A little later, `t2` writes a value to the same `IORef`.
- Finally, `t1` reads the `IORef` using `readIORef :: IORef a -> IO a`

The following diagrams will follow the same general struture: time increases
as we move downwards along a thread, and certain actions are taken within those threads.

`IORef`s are not very safe. They are highly succeptible to race conditions and other unintended behavior, and should be used with caution.
For example, in our diagram: `t2` modifies the `IORef` after `t1` wrote to it - `t1` probably expected that `readIORef` would return
whatever it placed there. That is not the case, because `t2` modified it between the write and read steps of `t1`.

- [Documentation for Data.IORef](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-IORef.html)

## MVar

<img src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/HaskellRefs/mvar.png" height="400" style="display:block;margin:auto"></img>

`MVar`s represent a location in memory that holds a value as well. However,
`MVar`s come with the guarantee that no two threads are modifying a variable at the same
time.

An `MVar` is either empty or full of an `a`. When we try to `takeMVar` on an
empty `MVar`, the current thread blocks (indicated by a black line) until a value is put back into the `MVar`. `GHC`'s
runtime is pretty good at determining when a thread is blocked indefinitely on an `MVar` read, so we don't often have
to worry about a thread hanging due to a bad program (for too long).

`MVar`s are still succeptible to race conditions, but are great for simple
concurrent tasks like synchronization and basic communication between threads.

- [Documentation for Control.Concurrent.MVar](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent-MVar.html)=
- [More on MVars](http://chimera.labs.oreilly.com/books/1230000000929/ch07.html)

## TVar

<img src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/HaskellRefs/tvar.png" height="400" style="display:block;margin:auto"></img>

`TVar`s solve a different problem. They are associated with a mechanism called Software Transactional Memory - `STM` -
- a construct that allows us to compose primitive operations
and run them sequentially as a _transaction_. Think database transaction: if one `STM` action in a chain fails, all previous actions taken in that chain are rolled back accordingly.

`TVar`s have a similar API to `MVar`, with one major difference: They can't ever be empty. `TVar`s can only be used in a singular thread, which is commonly executed as an atomic transaction using the function `atomically :: STM a -> IO ()`.

`STM` provides a bunch of very useful primitives for working with transactions, and is worth exploring:

- [Documentation for Control.Monad.STM](https://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Monad-STM.html)
- [Documentation for Control.Concurrent.STM.TVar](https://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TVar.html)
- [More on TVars and STM](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html)

## TMVar

<img src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/HaskellRefs/tmvar.png" height="400" style="display:block;margin:auto"></img>

This diagram should look pretty familiar! `TMVar`s are a mash between `TVar`s and
`MVar`s, as you might expect from its name. They can be composed transactionally
just like `TVar`s, but can also be empty, and shared across many threads.

Since all of these `TMVar` actions live in `STM`, they can be run in the same manner as when we use regular `TVar`s.

- [Documentation for Control.Concurrent.STM.TMVar](https://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TMVar.html)

## STRef

<img src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/HaskellRefs/stref.png" height="400" style="display:block;margin:auto"></img>

`STRef`s are a completely different type of mutable container. They are restricted to a single thread, much like `TVar`s,
but _guarantee_ that they never escape (they are thread-local). They live in a context called `ST`, indicating a stateful thread.

The `s` value in the type of `ST` and `STRef` is a reference to the thread that the `ST` computation is allowed to access.

`ST` and `STRef`s are mainly used to gain performance when you need
to be closer to memory, but don't want to give up safety.

- [More on ST/STRef](https://wiki.haskell.org/Monad/ST)
- [Documentation for Data.STRef](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-STRef.html)

Til next time!

Ben
