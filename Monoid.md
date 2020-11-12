---
date: 2020-09-22T15:40
---

# Monoid

A `Monoid` in [[Haskell]] is a data type with a lawful Monoid instance, that is:

```haskell
class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mappend = (<>)
```

`Monoid`s are data types that can be instantiated empty and be smashed
together without losing any information. A common example is and `[a]` (for
any `a`), where `mempty=[]` and `mappend=(++)`. Monoids are always
[[Semigroup]]s as well.
