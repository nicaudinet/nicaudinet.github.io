---
layout: post
title: Mixing Algebras
excerpt: Mixing two algebras together for use with recursion schemes
---

Before we begin, here are some nice resources on F-Algebras and recursion
schemes if you have not encountered them before or if you need a refresher like
I did:
- [Good refresher on recursion schemes in Haskell](https://medium.com/@jaredtobin/practical-recursion-schemes-c10648ec1c29)
- [Bartoz Milewski's blog post on F-Algebras](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras)
- [Awesome Recursion Schemes](https://github.com/passy/awesome-recursion-schemes)

A little trick I discovered to combine two algebras together

Effectively a "union" operation

Example: an algebra for summing values and an algebra for 

```haskell
{-# LANGUAGE DeriveFunctor #-}

module Data.Functor.Mix
  ( Mix(..)
  , mix
  , mixM
  ) where

data Mix f g x
  = F (f x)
  | G (g x)
  deriving Functor

mix :: (f a -> a) -> (g a -> a) -> Mix f g a -> a
mix f _ (F x) = f x
mix _ g (G x) = g x
```

## Mixing pure and monadic algebras

We can use the same trick to mix a "pure" algebra and a "monadic" algebra. By
"pure" and "monadic" here I mean algebras that are evaluated in a pure or
monadic context.

We can re-use the `Mix` data type from earlier, but we specify the second
functor to work over monadic values:

```haskell
mixM
  :: (Monad m, Traversable f)
  => (f a -> a)
  -> (g (m a) -> m a)
  -> Mix f g (m a)
  -> m a
mixM f _ (F x) = fmap f (sequence x)
mixM _ g (G x) = g x
```

For example, imagine we are building a DSL for a linear algebra library.
Initially, we want to support:
- Creating a new value
- Adding two values
- Multiplying two values

So, we go ahead and write a classic F-Algebra using `recursion-schemes` that
looks like this:

```haskell
data CalcF a x
  = LitF a
  | AddF x x
  | MulF x x
  deriving (Functor, Foldable, Traversable)

type Calc a = Fix (CalcF a)

instance Num a => Num (Calc a) where
  fromInteger = Fix . LitF . fromInteger
  x + y = Fix (AddF x y)
  x * y = Fix (MulF x y)
  abs = undefined
  signum = undefined
  negate = undefined

algebra :: Num a => CalcF a a -> a
algebra (LitF a) = a
algebra (AddF x y) = x + y
algebra (MulF x y) = x * y
```

Here, we:
1. Defined a non-recursive data type `CalcF` that represents the operations in
the DSL
2. Used `Fix` to define a recursive data type `Calc` based on `CalcF`
3. Defined a `Num` instance to easily construct expressions
4. Implement an algebra to evaluate our expression

With this plumbing done we can use it in combination with `cata` to evaluate
expressions:

```haskell
>>> cata algebra ((1 + 2) * 3 :: Calc Int)
>>> 9
```

But what if we want to introduce the ability to generate random numbers? It is
usually convenient to use a monadic context for random numbers which
automatically keeps track of the generator. The `MonadRandom` package provides
the `Rand` monad for exactly this purpose.

If we naively added this to our example above, it would look something like: 

```haskell
data CalcF a x
  = LitF a
  | AddF x x
  | MulF x x
  | RandF x x
  deriving (Functor, Foldable, Traversable)

type Calc a = Fix (CalcF a)

instance Num a => Num (Calc a) where
  fromInteger = Fix . LitF . fromInteger
  x + y = Fix (AddF x y)
  x * y = Fix (MulF x y)
  abs = undefined
  signum = undefined
  negate = undefined

rand :: Calc a -> Calc a -> Calc a
rand x y = Fix (RandF x y)

algebra :: (Num a, Random a, MonadRandom m) => CalcF a (m a) -> (m a)
algebra (LitF a) = pure a
algebra (AddF x y) = pure (x + y)
algebra (MulF x y) = pure (x * y)
algebra (RandF mx my) = do
  x <- mx
  y <- my
  getRandomR (x, y)
```

Then, we can evaluate expressions as so:

```haskell
>>> evalRandIO (cata algebra ((1 + 2) * rand 0 10) :: Rand StdGen (Calc Int))
>>> 15 
```

This works, but look at `algebra`: now, all the pure operations are evaluated in
a monadic context and we have to worry about putting `pure` everywhere. What if
instead we split our algebras into a pure and a monadic one and then combined
them together?

```haskell
data CalcF a x
  = LitF a
  | AddF x x
  | MulF x x
  deriving (Functor, Foldable, Traversable)

data RandF x
  = RandF x x
  deriving Functor

type CalcRand a = Fix (Mix (CalcF a) RandF)

instance Num a => Num (CalcRand a) where
  fromInteger = Fix . F . LitF . fromInteger
  x + y = Fix (F (AddF x y))
  x * y = Fix (F (MulF x y))
  abs = undefined
  signum = undefined
  negate = undefined

rand :: CalcRand a -> CalcRand a -> CalcRand a
rand x y = Fix (G (RandF x y))

calcAlgebra :: Num a => CalcF a a -> a
calcAlgebra (LitF a) = a
calcAlgebra (AddF x y) = x + y
calcAlgebra (MulF x y) = x * y

randAlgebra :: (MonadRandom m, Random a) => RandF (m a) -> m a
randAlgebra (RandF mx my) = do
  x <- mx
  y <- my
  getRandomR (x, y)

main :: IO ()
main = do
  let example = 1 + (rand 0 10) + 3 * (rand 0 (4 * 5)) :: CalcRand Int
  result <- evalRandIO $ cata (mixM calcAlgebra randAlgebra) example
  print result
```
