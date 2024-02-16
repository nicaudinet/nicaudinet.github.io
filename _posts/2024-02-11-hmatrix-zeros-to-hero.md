---
layout: post
title: Hmatrix - from zeros to hero
---

The goal of this post is to give a brief introduction to `hmatrix`'s Static API
and show how to implement a type-safe `zeros` function in two different ways.

I used GHC 9.4.8 throughout.

## The `hmatrix` Static API

[Hmatrix](https://hackage.haskell.org/package/hmatrix) is a Haskell library that
provides a nice functional interface for working with matrices. It is built on
top of [BLAS](https://www.netlib.org/blas/) and
[LAPACK](https://www.netlib.org/lapack/).

The library includes a [Static
API](https://hackage.haskell.org/package/hmatrix-0.20.2/docs/Numeric-LinearAlgebra-Static.html#v:build)
which allows one to define the size of the matrices at the type level. Here's
what creating a random 3x2 matrix looks like using the static API:

```haskell
{-# LANGUAGE DataKinds #-} -- For using type-level numbers

import Numeric.LinearAlgebra.Static

main :: IO ()
main = do
    x <- rand :: IO (L 3 2)
    print x
```

which gives the following output:

```
(matrix
 [ 0.33926174432936207,  0.7822155900216734
 ,  0.9783547953601716, 0.40214259103040334
 , 0.15344813892312728,  0.2366858740508025 ] :: L 3 2)
```

The benefit of storing the size at the type level is that **the compiler can use
the information to prevent impossible operations at compile time**. For example,
trying to compile this program (where `(<>)` is matrix multiplication):

```haskell
main :: IO ()
main = do
    x <- rand :: IO (L 3 2)
    y <- rand :: IO (L 5 10)
    print (x <> y)
```

results in the following compiler error:

```
exe/Main.hs:24:18: error:
    • Couldn't match type ‘5’ with ‘2’
      Expected: L 2 10
        Actual: L 5 10
    • In the second argument of ‘(LA.<>)’, namely ‘y’
      In the first argument of ‘print’, namely ‘(x LA.<> y)’
      In a stmt of a 'do' block: print (x LA.<> y)
   |
24 |   print (x LA.<> y)
   |
```

That is, the compiler is telling us that it is impossible to multiply a 3x2
matrix by a 5x10 matrix since 2 != 5. Admittedly, the ergonomics of the type
error are not great, but the guarantees are nice!

On top of that, keeping the size information at the type level means we don't
need to check for different input sizes in the body of the function: the size of
the inputs is guaranteed by the compiler.

However, the `hmatrix` static API suffers from lack of development and
documentation. In the rest of this post we will close the gap a little by
implementing the `zeros` function from Python's `numpy` two ways.

## The `zeros` function in two ways

[`numpy.zeros`](https://numpy.org/doc/stable/reference/generated/numpy.zeros.html)
in Python is a common function for creating matrices with all elements set to 0.
However, this function is missing from the `Hmatrix` Static API. The goal of
this section is to implement it in two ways. The first method introduces some
key type-level programming patters for working with the Static API. The second
method shows a cleaner alternative and is included for completeness.

The type of the `zeros` function we want to build is:

```haskell
import Numeric.LinearAlgebra.Static

zeros :: L n m
zeros = undefined
```

In other words, the `zeros` function returns a matrix with all elements set to 0
of size NxM.

### Implementation using `matrix`

Our first strategy for implementing `zeros` will be to make a wrapper around the
unsafe
[`matrix`](https://hackage.haskell.org/package/hmatrix-0.20.2/docs/Numeric-LinearAlgebra-Static.html#v:matrix)
function from the Static API:

```haskell
matrix :: (KnownNat m, KnownNat n) => [Double] -> L n m
```

The `matrix` function takes a list of `Double`s of size N*M and returns a matrix
of size NxM. This function is unsafe, because giving `matrix` a list of the wrong
size results in a runtime error. To solve this, we will make use of `natVal`
from
[`GHC.TypeLits`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-TypeLits.html#v:natVal),
which allows us to bring information from the type level to the value level. For
example:

```
ghci> :set -XDataKinds -XTypeApplications
ghci> import Data.Proxy
ghci> import GHC.TypeLits
ghci> x = Proxy @5
ghci> y = Proxy @6
ghci> natVal x + natVal y
11
```

Here, we first create two values `x` and `y` using `Proxy`. From [the
docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Proxy.html):

> Proxy is a type that holds no data, but has a phantom parameter of arbitrary
> type (or even kind). Its use is to provide type information, even though there
> is no value available of that type (or it may be too costly to create one).

In the example, we use `Proxy` to carry numerical information at the type
level: `x` carries the number 5 and `y` carries the number `6`. We can then use
`natVal` to bring the numbers carried by the proxies from the type level to the
value level and add them together. We also used the
[`TypeApplications`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_applications.html?highlight=typeapplication#extension-TypeApplications)
language extension to use the `@` notation for specifying the value of type
variables.

In the `zeros` function, we can use `natVal` in order to automatically create a
list of the correct size using the standard `replicate` function.

Let's take a first stab at it:

```haskell
{-# LANGUAGE TypeApplications #-}

import Numeric.LinearAlgebra.Static
import GHC.TypeLits (natVal)
import Data.Data (Proxy(..))

zeros :: L n m
zeros =
  let n_val = fromIntegral (natVal @n Proxy)
      m_val = fromIntegral (natVal @m Proxy)
   in matrix (replicate (n_val * m_val) 0)
```

Trying to run this code gives us the following compiler error:

```
src/Const/Matrix.hs:13:37: error: Not in scope: type variable ‘n’
   |
13 |   let n_val = fromIntegral (natVal @n Proxy)
   |                                     ^
src/Const/Matrix.hs:14:37: error: Not in scope: type variable ‘m’
   |
14 |       m_val = fromIntegral (natVal @m Proxy)
   |                                     ^
```

This error occurs because by default the compiler does not recognise that the
type variable `n` in the `let` binding is the same as the type variable `n` in
the function's type definition (and the same with `m`). To fix this we can use
the
[`ScopedTypeVariables`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/scoped_type_variables.html?highlight=scopedtypevariables#extension-ScopedTypeVariables)
language extension which allows the body of the function to access type
variables from the function's type definition when they are explicitly
introduced with the `forall` keyword:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Numeric.LinearAlgebra.Static
import GHC.TypeLits (natVal)
import Data.Data (Proxy(..))

zeros :: forall n m. L n m
zeros =
  let n_val = fromIntegral (natVal @n Proxy)
      m_val = fromIntegral (natVal @m Proxy)
   in matrix (replicate (n_val * m_val) 0)
```

Now we get another scary-looking set of compiler errors:

```
src/Const/Matrix.hs:13:29-34: error:
    • No instance for (KnownNat n) arising from a use of ‘natVal’
      Possible fix:
        add (KnownNat n) to the context of
          the type signature for:
            zeros :: forall (n :: GHC.TypeNats.Nat) (m :: GHC.TypeNats.Nat).
                     L n m
    • In the first argument of ‘fromIntegral’, namely
        ‘(natVal @n Proxy)’
      In the expression: fromIntegral (natVal @n Proxy)
      In an equation for ‘n_val’: n_val = fromIntegral (natVal @n Proxy)
   |
13 |   let n_val = fromIntegral (natVal @n Proxy)
   |                             ^^^^^^
src/Const/Matrix.hs:14:29-34: error:
    • No instance for (KnownNat m) arising from a use of ‘natVal’
      Possible fix:
        add (KnownNat m) to the context of
          the type signature for:
            zeros :: forall (n :: GHC.TypeNats.Nat) (m :: GHC.TypeNats.Nat).
                     L n m
    • In the first argument of ‘fromIntegral’, namely
        ‘(natVal @m Proxy)’
      In the expression: fromIntegral (natVal @m Proxy)
      In an equation for ‘m_val’: m_val = fromIntegral (natVal @m Proxy)
   |
14 |       m_val = fromIntegral (natVal @m Proxy)
   |                             ^^^^^^
src/Const/Matrix.hs:15:7-12: error:
    • No instance for (KnownNat n) arising from a use of ‘matrix’
      Possible fix:
        add (KnownNat n) to the context of
          the type signature for:
            zeros :: forall (n :: GHC.TypeNats.Nat) (m :: GHC.TypeNats.Nat).
                     L n m
    • In the expression: matrix (replicate (n_val * m_val) 0)
      In the expression:
        let
          n_val = fromIntegral (natVal @n Proxy)
          m_val = fromIntegral (natVal @m Proxy)
        in matrix (replicate (n_val * m_val) 0)
      In an equation for ‘zeros’:
          zeros
            = let
                n_val = fromIntegral (natVal @n Proxy)
                m_val = fromIntegral (natVal @m Proxy)
              in matrix (replicate (n_val * m_val) 0)
   |
15 |    in matrix (replicate (n_val * m_val) 0)
   |       ^^^^^^
```

Here, the compiler is saying that it doesn't know whether the type variables `n`
and `m` are associated with a particular integer. This can be fixed by giving
them a [`KnownNat`
constraint](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_literals.html#runtime-values-for-type-level-literals):

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Numeric.LinearAlgebra.Static
import GHC.TypeLits (natVal, KnownNat)
import Data.Data (Proxy(..))

zeros :: forall n m. (KnownNat n, KnownNat m) => L n m
zeros =
  let n_val = fromIntegral (natVal @n Proxy)
      m_val = fromIntegral (natVal @m Proxy)
   in matrix (replicate (n_val * m_val) 0)
```

And voilá! We have implemented a type-safe function to create constant matrices
using the `hmatrix` static API. Here's a complete program that creates and
prints a matrix full of zeros:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Data (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)
import Numeric.LinearAlgebra.Static as LA

zeros :: forall n m. (KnownNat m, KnownNat n) => L n m
zeros =
  let n_val = fromIntegral (natVal @n Proxy)
      m_val = fromIntegral (natVal @m Proxy)
   in matrix (replicate (n_val * m_val) 0)

main :: IO ()
main = print (zeros @3 @2)
```

which returns:

```
(matrix
 [ 0.0, 0.0
 , 0.0, 0.0
 , 0.0, 0.0 ] :: L 3 2)
```

### Implementation using `build`

A more straightforward (but less fun) way to implement the `zeros` function in
Hmatrix is using the `build` function from the Static API:

```haskell
build :: forall m n. (KnownNat n, KnownNat m)
    => (Double -> Double -> Double) -> L m n
```

`build` is a higher-order function which takes a function as input and returns a
matrix of the correct size. The input function takes two arguments (the row and
column index of the element) and returns a value. The `build` function then
creates the matrix of the correct size (using similar `forall` and `natVal`
tricks as before) and creates the values by applying the input function to the
indices of each cell. Therefore, to implement `zeros` all we need to do is
define a function that takes two arguments of type `Double` and always returns
0 and pass it as an argument to `build`:

```haskell
constZero :: Double -> Double -> Double
constZero _ _ = 0

zeros :: (KnownNat n, KnownNat m) => L n m
zeros = build constZero
```

Here's the full program:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import GHC.TypeLits (KnownNat)
import Numeric.LinearAlgebra.Static

constZero :: Double -> Double -> Double
constZero _ _ = 0

zeros :: (KnownNat n, KnownNat m) => L n m
zeros = build constZero

main :: IO ()
main = print (zeros @3 @2)
```

which returns:

```
(matrix
 [ 0.0, 0.0
 , 0.0, 0.0
 , 0.0, 0.0 ] :: L 3 2)
```
