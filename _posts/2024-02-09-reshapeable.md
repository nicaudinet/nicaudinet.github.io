---
layout: post
title: Typesafe reshape with Hmatrix
---

The goal of this post is to show how to implement a type-safe `reshape` function
using Hmatrix's Static API. The first section presents a quick introduction to
Hmatrix's Static API for the uninitiated. The second section shows how to
implement a function to create matrices with constant elements in a type-safe
way. The third and final section shows how to implement the `squash`, `unsquash`
and `reshape` in a type-safe way.

## Hmatrix

[Hmatrix](https://hackage.haskell.org/package/hmatrix) is a Haskell library that
provides a nice functional interface for working with matrices. It is built on
top of BLAS and LAPACK.

The library also includes an experimental Static API which allows one to define
the size of the matrices in the type. The library can then use this information
to check various properties of the matrix operations at compile time. The static
API makes use of the
[DataKinds](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/data_kinds.html#extension-DataKinds)
language extension for ergonomically dealing with numbers at the type level. 

Here's what creating a random 3x2 matrix looks like with Hmatrix's Static API:

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

Where the Static API starts to show its value is when  ... to be continued

## Creating constant matrices

## Reshapeable
