---
title: Haskell Bits #1: Randomness
author: Benjamin Kovach
tags:
  - Haskell
---

Haskell Bits is a new series of bite-sized posts that I hope will empower people to "get it done and move on", providing
useful information and links to learn more if desired. I'll be providing
full `main` files in each example (with imports!) to make porting this stuff into your own project as frictionless as possible.
This first "Haskell Bit" will cover randomness.

You need at least two things to produce a random number:

- An initial "seed" value
- A pure function that produces a new number from that seed. ("RNG")

That's all for a single number.

Most programming languages will hide these details from you unless you need them.
Most of the time, you can just call `random()` and get a random number (typically between 0 and 1), using a seed value
generated from some system variable that is always changing (current time in very small units is common).

The simplest way to replicate this behavior in Haskell is by using the `System.Random` module, part of the [`random`](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html) package.

We can use `randomIO` and `randomRIO` to pull from a global RNG:

```haskell
import System.Random

main :: IO ()
main = do
  -- A random `Double` between 0 and 1
  (randomIO :: IO Double) >>= print

  -- A random `Int` between 1 and 6 (A die roll)
  randomRIO (1, 6) >>= print
```

This is pretty much the interface that most other languages start with. Better would be to separate out IO as much as possible
from the inevitable rest of our program. We can do that by confining IO usage to one operation: coming up with an initial RNG.

```haskell
import System.Random

dieRoll :: RandomGen g => g -> (Int, g)
dieRoll = randomR (1, 6)

main :: IO ()
main = do
  -- New generator, generated from the system RNG 
  gen <- newStdGen 

  let (result, newGen) = dieRoll gen
  print result

  let (newResult, newNewGen) = dieRoll newGen
  print newResult 
```

We don't want to duplicate this code every time we want to add a new die roll.
The next logical step would be to sprinkle in some `State` to store the current RNG in:

```haskell
import System.Random
import Control.Monad.State

dieRoll :: RandomGen g => State g Int
dieRoll = state (randomR (1, 6))

twoDice :: RandomGen g => State g Int
twoDice = (+) <$> dieRoll <*> dieRoll

main :: IO ()
main = do
  gen <- newStdGen
  print (evalState twoDice gen)
```

Now we can run more complex programs that employ random numbers. Note that `newStdGen` can be
replaced with `mkStdGen :: Int -> StdGen` if you want to provide an integral seed instead of using the global
`StdGen`.

You can avoid some of the state boilerplate and get a few more benefits by bringing in the [`MonadRandom`](https://hackage.haskell.org/package/MonadRandom-0.5) package. Here's some
code that accomplishes the same goal using `MonadRandom`:

```haskell
import System.Random
import Control.Monad.Random

dieRoll :: RandomGen g => Rand g Int
dieRoll = getRandomR (1, 6)

twoDice :: RandomGen g => Rand g Int
twoDice = (+) <$> dieRoll <*> dieRoll

main :: IO ()
main = do
  gen <- newStdGen
  print (evalRand twoDice gen)
```

Apart from providing a nice way to write (slightly) terser randomness code, `MonadRandom` is more explicit about the domain we're working in, and ships with a couple of killer utilities; namely, the minimalistic
sampling functions `uniform` and `fromList` (also `weighted` from `MonadRandom 0.5`). This program, for example, generates
a list of 20 moves that might come up in a Dance Dance Revolution song:

```haskell
import Control.Monad
import Control.Monad.Random
import System.Random

data Direction = U | D | L | R deriving (Show, Eq)

step :: RandomGen g => Rand g Direction
step = uniform [U,D,L,R]

stepWeighted :: RandomGen g => Rand g Direction
stepWeighted =
    fromList [(U, 1), (D, 1), (L, 50), (R, 100)]

danceDanceRevolutionScroll :: RandomGen g => Rand g [Direction]
danceDanceRevolutionScroll = replicateM 20 $ do
    weightIt <- uniform [True, False]
    if weightIt then stepWeighted else step

main :: IO ()
main = do
  gen <- newStdGen
  print (evalRand danceDanceRevolutionScroll gen)
```

`fromList` lets you specify weights for your random elements. `L` and `R` will probably show up a lot more than the other two
directions when this is run. 

`MonadRandom` supplies some other conveniences as well, but it's not crazy stuffed with functionality. It's a nice package that contains
the minimal amount of code to be useful but not overengineered.

That said, sometimes you need more. First off, what about different distributions? The normal distribution is a pretty common necessity.
[`random-fu`](https://hackage.haskell.org/package/random-fu) really shines in this domain.
You'll have to pull in the [`rvar`](https://hackage.haskell.org/package/rvar) package as well to run this next example, which will print out
a random number pulled from a normal distribution with mean `100` and a standard deviation of `5`:

```haskell
import Control.Monad
import System.Random
import Data.Random
import Data.RVar
import Control.Monad.State

normalNumber :: State StdGen Double
normalNumber = sampleRVar (normal 100 5)

main :: IO ()
main = do
  gen <- newStdGen
  print (evalState normalNumber gen)
```

Notice the `State` pattern from earlier. Also, there a bunch of common distributions that ship with `random-fu`.

One last thing I should mention is that we're not tied to `StdGen`, the RNG that ships with `random`.

In fact, it does not have strong statistical properties, and should probably be avoided for many "real" applications (See [this reddit post](https://www.reddit.com/r/haskell/comments/3x15sm/why_is_the_first_random_value_produced_from_a/), and thank you to reddit user tom-md for the note!).

There are faster and more stable ones
ones, like `PureMT` from [`random-source`](https://hackage.haskell.org/package/random-source) or `TFGen` from [`tf-random`](https://hackage.haskell.org/package/tf-random).
These are both instances of `RandomGen`, so you can plug either one
of those in wherever you saw the generic type signature `RandomGen g => ...` in this post. For example, mixing `PureMT`
back into `MonadRandom`:

```haskell
import Control.Monad.Random
import Data.Random.Source.PureMT

dieRoll :: RandomGen g => Rand g Int
dieRoll = getRandomR (1, 6)

main :: IO ()
main = newPureMT >>= print . evalRand dieRoll 
```

Ben
