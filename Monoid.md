---
date: 2020-09-22T15:40
---

# Monoid

A `Monoid` in [[Haskell]] is a data type with a lawful Monoid instance, that is:

```haskell
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
```

In a handwavy sense, `Monoid`s are data types that can be instantiated empty and
be smashed together without losing any information. A common example is and
`[a]` (for any `a`), where `mempty=[]` and `mappend=(++)`.