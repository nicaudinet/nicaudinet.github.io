---
layout: post
title: Type-safe constant matrices with Hmatrix
---

The goal of this post is to show how to implement a type-safe `const` function
to create matrices with a constant value using Hmatrix's Static API. The first
section presents a quick introduction to Hmatrix's Static API for the
uninitiated. The second section shows how to iteratively implement the function.

I used GHC 9.4.8.

## Hmatrix static API

[Hmatrix](https://hackage.haskell.org/package/hmatrix) is a Haskell library that
provides a nice functional interface for working with matrices. It is built on
top of [BLAS](https://www.netlib.org/blas/) and
[LAPACK](https://www.netlib.org/lapack/).

The library also includes an experimental Static API which allows one to define
the size of the matrices at the type level. The library can then use this
information to check various properties of the matrix operations at compile
time. The static API makes use of the
[DataKinds](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/data_kinds.html#extension-DataKinds)
language extension for ergonomically dealing with type-level numbers. 

Here's what creating a random 3x2 matrix looks like using the static API:

```haskell
{-# LANGUAGE DataKinds #-}

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

"Why bother with this type-level size stuff" you may ask? In my view, there are
two big benefits:

1. **The compiler can use the size information to prevent impossible operations
   at compile time**. For example, trying to compile this program (where `(<>)`
   is matrix multiplication):

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
    error is not great, but the guarantees are nice.

2. **The size of the matrices becomes more explicit**. In this case, I'm
   comparing the eronomics of `hmatrix` to the ergonomics of `numpy`. In Python,
   if we want to know the size of a matrix we need to either run the code and
   check with a print statement or some other tool, or read through the code to
   figure out what it's doing. In small projects this is not a big problem, but
   can it can quickly become annoying when dealing with larger code bases or
   other people's poorly documented libraries. In `hmatrix` however, all you
   need to do is go to the type definition. There's even an LSP function for
   that.

However, the `hmatrix` static API suffers from lack of development and doesn't
implement many convenience functions that are normal in `numpy`. In the rest of
this blog post we will close the gap a little by implementing one of these
missing functions.

## Creating constant matrices

A very common operation when using `numpy` is creating matrices with constant
elements (for example using
[`numpy.zeros`](https://numpy.org/doc/stable/reference/generated/numpy.zeros.html)).
Here, we will implement a type-safe version of this function using the
`hmatrix` static API.

The type of the function we want to implement is the following:

```haskell
import Numeric.LinearAlgebra.Static

const :: Double -> L m n
const value = undefined
```

In other words, given a value of type `Double` (all matrices in
`hmatrix` have elements of type `Double`) we get a matrix of size MxN with all
elements equal to the value.

**Note**: The name `const` clashes with another function defined in `Prelude`.
To get around this you can explicitly import the Prelude with `import Prelude
hiding (const)`. Another (probably better) alternative would be to give our
function a different name.

Our strategy for implementing `const` will be to make a wrapper around the
unsafe
[`matrix`](https://hackage.haskell.org/package/hmatrix-0.20.2/docs/Numeric-LinearAlgebra-Static.html#v:matrix)
function from `hmatrix` which takes a list of values of size MxN and returns a
matrix. On its own, this function is unsafe because giving `matrix` a list of
the wrong size results in a runtime error. To solve this, we will make use of
the
[`TypeApplications`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_applications.html?highlight=typeapplication#extension-TypeApplications)
language extension and `natVal` and `Proxy` from
[`GHC.TypeLits`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-TypeLits.html#v:natVal)
to get M and N from the type level to the value level. Then, we will use
`replicate (n * m) value` to construct a list which is guaranteed to have the
correct size. Let's have a go at it:

```haskell
{-# LANGUAGE TypeApplications #-}

import Numeric.LinearAlgebra.Static
import GHC.TypeLits (natVal)
import Data.Data (Proxy(..))

const :: Double -> L m n
const value =
  let n = fromIntegral (natVal @n Proxy)
      m = fromIntegral (natVal @m Proxy)
   in matrix (replicate (n * m) value)
```

However, this gives us the following compiler error:

```
exe/Main.hs:19:33: error: Not in scope: type variable ‘n’
   |
19 |   let n = fromIntegral (natVal @n Proxy)
   |                                 ^
exe/Main.hs:20:33: error: Not in scope: type variable ‘m’
   |
20 |       m = fromIntegral (natVal @m Proxy)
   |
```

This error occurs because by default the compiler does not recognise that the
type variable `n` in the `let` binding is the same as the `n` in the type
definition of the function. To remedy this we can use the
[`ScopedTypeVariables`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/scoped_type_variables.html?highlight=scopedtypevariables#extension-ScopedTypeVariables)
language extension:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Numeric.LinearAlgebra.Static
import GHC.TypeLits (natVal)
import Data.Data (Proxy(..))

const :: forall n m. Double -> L m n
const value =
  let n = fromIntegral (natVal @n Proxy)
      m = fromIntegral (natVal @m Proxy)
   in matrix (replicate (n * m) value)
```

Now we get another scary-looking compiler error:

```
exe/Main.hs:19:25-30: error:
    • No instance for (KnownNat n) arising from a use of ‘natVal’
      Possible fix:
        add (KnownNat n) to the context of
          the type signature for:
            const :: forall (n :: GHC.TypeNats.Nat)
                                  (m :: GHC.TypeNats.Nat).
                           Double -> L m n
    • In the first argument of ‘fromIntegral’, namely
        ‘(natVal @n Proxy)’
      In the expression: fromIntegral (natVal @n Proxy)
      In an equation for ‘n’: n = fromIntegral (natVal @n Proxy)
   |
19 |   let n = fromIntegral (natVal @n Proxy)
   |                         ^^^^^^
exe/Main.hs:20:25-30: error:
    • No instance for (KnownNat m) arising from a use of ‘natVal’
      Possible fix:
        add (KnownNat m) to the context of
          the type signature for:
            const :: forall (n :: GHC.TypeNats.Nat)
                                  (m :: GHC.TypeNats.Nat).
                           Double -> L m n
    • In the first argument of ‘fromIntegral’, namely
        ‘(natVal @m Proxy)’
      In the expression: fromIntegral (natVal @m Proxy)
      In an equation for ‘m’: m = fromIntegral (natVal @m Proxy)
   |
20 |       m = fromIntegral (natVal @m Proxy)
   |                         ^^^^^^
exe/Main.hs:21:7-12: error:
    • No instance for (KnownNat m) arising from a use of ‘matrix’
      Possible fix:
        add (KnownNat m) to the context of
          the type signature for:
            const :: forall (n :: GHC.TypeNats.Nat)
                                  (m :: GHC.TypeNats.Nat).
                           Double -> L m n
    • In the expression: matrix (replicate (n * m) value)
      In the expression:
        let
          n = fromIntegral (natVal @n Proxy)
          m = fromIntegral (natVal @m Proxy)
        in matrix (replicate (n * m) value)
      In an equation for ‘const’:
          const value
            = let
                n = fromIntegral (natVal @n Proxy)
                m = fromIntegral (natVal @m Proxy)
              in matrix (replicate (n * m) value)
   |
21 |    in matrix (replicate (n * m) value)
   |
```

Here, the copiler is saying that it doesn't know that the type variables `n` and
`m` actually correspond to numerical values (they do not have an instance for
`KnownNat`). Let's give them one:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Numeric.LinearAlgebra.Static
import GHC.TypeLits (natVal, KnownNat)
import Data.Data (Proxy(..))

const :: forall n m. (KnownNat m, KnownNat n) => Double -> L m n
const value =
  let n = fromIntegral (natVal @n Proxy)
      m = fromIntegral (natVal @m Proxy)
   in matrix (replicate (n * m) value)
```

And voilá! We have implemented a type-safe function to create constant matrices
using the `hmatrix` static API. Now we can use this to recreate `numpy.zeros`:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Data (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)
import Numeric.LinearAlgebra.Static as LA

const :: forall n m. (KnownNat m, KnownNat n) => Double -> L m n
const value =
  let n = fromIntegral (natVal @n Proxy)
      m = fromIntegral (natVal @m Proxy)
   in matrix (replicate (n * m) value)

zeros :: forall n m. (KnownNat n, KnownNat m) => L n m
zeros = const 0

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
