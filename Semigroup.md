---
date: 2020-11-12
---

# Semigroup

A [[Monoid]] without an identity element. In [[Haskell]]:

```haskell
class Semigroup s where
  (<>) :: s -> s -> s
```
