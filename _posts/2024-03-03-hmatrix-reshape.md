---
layout: post
title: Reshape in Hmatrix
excerpt: A post where I explain and implement a type-safe matrix reshape function with the Hmatrix Static API
---

The goal of this post is to implement a type-safe `reshape` function using the
[Hmatrix Static
API](https://hackage.haskell.org/package/hmatrix-0.20.2/docs/Numeric-LinearAlgebra-Static.html).

I used GHC 9.4.8 throughout this post. I also had to enable a whole menagerie of
language extensions:
- [DataKinds](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/data_kinds.html#extension-DataKinds)
- [TypeOperators](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_operators.html#extension-TypeOperators)
- [MultiParamTypeClasses](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/multi_param_type_classes.html#extension-MultiParamTypeClasses)
- [FlexibleInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-FlexibleInstances)
- [FlexibleContexts](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/flexible_contexts.html#extension-FlexibleContexts)
- [UndecideableInstances](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-UndecidableInstances)
- [ScopedTypeVariables](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables)
- [TypeApplications](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_applications.html#extension-TypeApplications)

If you're not familiar with the Hmatrix Static API, I've also made [a more
introductory
post](https://nicaudinet.github.io/2024/02/11/hmatrix-zeros-to-hero/) about
implementing the `zeros` function with the Hmatrix Static API.

# The goal

[Reshape](https://numpy.org/doc/stable/reference/generated/numpy.reshape.html)
is a common Numpy function that gives a new shape to a multi-dimensional array
without changing the data. An example from the docs:

```python
>>> a = np.arange(6).reshape((3, 2))
>>> a
array([[0, 1],
       [2, 3],
       [4, 5]])
```

At the moment, the Hmatrix Static API only supports vectors (1D) and matrices
(2D). Since vectors are 1D they cannot be reshaped while maintaining the same
elements. Functions to embed vectors into matrices and vice versa already exist
in Hmatrix, although they are limited to single-row matrices:
- `row :: R n -> L 1 n`: transform a vector into a single-row matrix
- `unrow :: L 1 n -> R n`: transform a single-row matrix into a vector

What doesn't exist yet in Hmatrix is a `reshape` function which transforms a
matrix into another matrix of a different shape. More specifically, there isn't
a function that takes a matrix `L n m` and tranfroms it into another matrix `L p
q` where `n * m = p * q`. So let's make it!

Our strategy for implementing `reshape` will be to first implement two other
functions:
- `flatten :: L n m -> R (n * m)`: a more general version of `unrow` which takes
  a matrix with any number of rows and transforms it into vector
- `unflatten :: R (n * m) -> L n m`: a more general version of `row` which takes
  a vector and wraps it into a matrix

`reshape` will then be a defined as a composition of `flatten` and `unflatten`,
which will end up looking something like this:

```haskell
reshape :: (n * m ~ p * q) => L n m -> L p q
reshape = unflatten . flatten
```

For the purposes of this post, we will work in a row-wise fashion rather than
column-wise. For example, row-wise flattening of a 3x3 matrix will look like
this:

![flatten.svg]({{ site.baseurl }}public/reshape-in-hmatrix/flatten.svg){:width="80%" style="display:block; margin-left:auto; margin-right:auto"}

and unflattening the array into a 3x3 matrix row-wise looks like this:

![unflatten.svg]({{ site.baseurl }}public/reshape-in-hmatrix/unflatten.svg){:width="80%" style="display:block; margin-left:auto; margin-right:auto"}

# The `flatten` function

The `flatten` function can be thought of as a fold which recursively takes the
top row of the matrix and concatentates it in front of the flattened version of
the rest of the matrix. The recursion stops when we encounter a matrix with a
single row, in which case we can just use the `unrow` function from before. We
can use `natVal` and `Proxy` to inspect the size of the matrix at the value
level to decide whether to stop the recursion, and `splitRows` and `(#)` from
the Static API to split the matrix and concatenate vectors, respsectively.
Bringing all of this together gives us our first implementation:

```haskell
import Data.Proxy
import GHC.TypeLits
import Numeric.LinearAlgebra.Static

flatten :: forall n m. (KnownNat n, KnownNat m) => L n m -> R (n * m)
flatten mat =
  case natVal (Proxy @n) of
    1 -> unrow mat
    _ ->
      let (r, rest) = splitRows m :: (L 1 m, L (n - 1) m)
       in unrow r # flatten rest
```

Unfortunately, this doesn't work. We get the following compiler error:

```
src/Reshape/Flatten.hs:19:16-18: error:
    • Couldn't match type ‘n’ with ‘1’
      Expected: L 1 m
        Actual: L n m
      ‘n’ is a rigid type variable bound by
        the type signature for:
          flatten :: forall (n :: Nat) (m :: Nat).
                     (KnownNat n, KnownNat m) =>
                     L n m -> R (n * m)
        at src/Reshape/Flatten.hs:16:1-71
    • In the first argument of ‘unrow’, namely ‘mat’
      In the expression: unrow mat
      In a case alternative: 1 -> unrow mat
    • Relevant bindings include
        mat :: L n m (bound at src/Reshape/Flatten.hs:17:9)
        flatten :: L n m -> R (n * m)
          (bound at src/Reshape/Flatten.hs:17:1)
   |
19 |     1 -> unrow mat
   |                ^^^
src/Reshape/Flatten.hs:21:23-31: error:
    • Cannot satisfy: 1 <= n
    • In the expression: splitRows mat :: (L 1 m, L (n - 1) m)
      In a pattern binding:
        (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
      In the expression:
        let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
        in unrow r # flatten rest
   |
21 |       let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
   |                       ^^^^^^^^^
```

These errors are essentially telling us that, due to a limitation of the Haskell
type system, we cannot implement the logic of choosing the recursive or
non-recursive step at the value level. We will get similar errors if we try to
implement `unflatten` in the same way. Instead, we need to encode the logic of
the `case` statement at the type level, for which we will need to use type
classes and some trickery.

# The `Flattenable` type class

This time, we will implement `flatten` as a method in a type class called
`Flattenable`:

```haskell
class Flattenable n m where
  flatten :: L n m -> R (n * m)
```

The type class has two parameters, `n` and `m`, which represent the size of the
matrix being flattened. We will also implement two instances for `Flattenable`:

```haskell
-- the single-row case
instance Flattenable 1 m where
  flatten = unrow

-- the multi-row case
instance {-# OVERLAPS #-} Flattenable n m where
  flatten = undefined
```

These two instances are the key to encoding the `case` statement logic from
before at the type level. When the compiler sees a call to `flatten`, it will
try to match the type of the input matrix to an appropriate instance. If the
input matrix has more than one row then the compiler will match the second
instance (which matches matrices of any size). If the matrix has a single row
then the compiler will match both instances. This is a problem, because if the
compiler has more than one option to choose from it becomes unclear which one it
should choose (and we want compilation to be predictable so the compiler can't
randomly choose one of them). One way to get around this is to use the [{-#
OVERLAPS
#-}](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#pragma-OVERLAPPABLE)
pragma. This tells the compiler that, in the case where both instances match, it
can ignore the second instance in favor of the first one.

Implementing the instance for single-row matrices is straightforward: it just
becomes a call to `unrow`. Implementing the instance for multi-row matrcies is a
bit trickier however. Our strategy will be the same as before: we will flatten
the matrix by recursively taking the top row and concatenating it to the
flattened version of the rest of the matrix. Here's our second attempt:

```haskell
instance {-# OVERLAPS #-} Flattenable n m where
  flatten mat =
    let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
     in unrow r # flatten rest
```

Again, this doesn't work. Compiling this code gives us the following errors:

```
src/Reshape/Flatten.hs:49:21-29: error:
    • Cannot satisfy: 1 <= n
    • In the expression: splitRows mat :: (L 1 m, L (n - 1) m)
      In a pattern binding:
        (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
      In the expression:
        let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
        in unrow r # flatten rest
   |
49 |     let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
   |                     ^^^^^^^^^
src/Reshape/Flatten.hs:50:9-30: error:
    • Couldn't match type: m + ((n - 1) * m)
                     with: n * m
      Expected: R (n * m)
        Actual: R (m + ((n - 1) * m))
    • In the expression: unrow r # flatten rest
      In the expression:
        let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
        in unrow r # flatten rest
      In an equation for ‘flatten’:
          flatten mat = let (r, rest) = ... in unrow r # flatten rest
    • Relevant bindings include
        r :: L 1 m (bound at src/Reshape/Flatten.hs:49:10)
        rest :: L (n - 1) m (bound at src/Reshape/Flatten.hs:49:13)
        mat :: L n m (bound at src/Reshape/Flatten.hs:48:11)
        flatten :: L n m -> R (n * m)
          (bound at src/Reshape/Flatten.hs:48:3)
   |
50 |      in unrow r # flatten rest
   |         ^^^^^^^^^^^^^^^^^^^^^^
```

The first error says that the compiler doesn't known that the type variable `n`
is greater than 0, which it needs to be since we call `flatten` on `m2 :: L
(n-1) m` and we can't have a matrix with a negative number of rows. To fix the
error we need to add a `1 <= n` constraint to prevent the compiler from matching
the instance with matrices with 0 rows. 

The second error tells us that the compiler doesn't know that the equality `m +
((n-1) * m) = n * m` holds for numbers at the type level. By default the Haskell
type system is not great at simple algebraic proofs! Thankfully we can convince
the type checker that the equality holds with [equality
constraints](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/equality_constraints.html)),
where `t1 ~ t2` means that type `t1` is the same as type `t2`. 

```haskell
instance
  {-# OVERLAPS #-}
  ( 1 <= n,
    m + ((n - 1) * m) ~ n * m
  ) =>
  Flattenable n m where

  flatten mat =
    let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
     in unrow r # flatten rest
```

Compiling again we get several new errors. Let's tackle them one by one:

```
src/Reshape/Flatten.hs:52:21-29: error:
    • Could not deduce (KnownNat n) arising from a use of ‘splitRows’
      from the context: (1 <= n, (m + ((n - 1) * m)) ~ (n * m))
        bound by the instance declaration
        at src/Reshape/Flatten.hs:(36,3)-(49,17)
      Possible fix:
        add (KnownNat n) to the context of the instance declaration
    • In the expression: splitRows mat :: (L 1 m, L (n - 1) m)
      In a pattern binding:
        (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
      In the expression:
        let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
        in unrow r # flatten rest
   |
52 |     let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
   |                     ^^^^^^^^^
```

This first error tell us that the compiler doesn't know that `n` corresponds to
a number (i.e. that it belongs to the `KnownNat` type class). We can fix this by
adding a `KnownNat n` constraint to the instance.

```
src/Reshape/Flatten.hs:53:17: error:
    • Could not deduce (KnownNat m) arising from a use of ‘#’
      from the context: (1 <= n, (m + ((n - 1) * m)) ~ (n * m))
        bound by the instance declaration
        at src/Reshape/Flatten.hs:(36,3)-(49,17)
      Possible fix:
        add (KnownNat m) to the context of the instance declaration
    • In the expression: unrow r # flatten rest
      In the expression:
        let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
        in unrow r # flatten rest
      In an equation for ‘flatten’:
          flatten mat = let (r, rest) = ... in unrow r # flatten rest
   |
53 |      in unrow r # flatten rest
   |                 ^
```

Similarly, the compiler also doesn't know that `m` corresponds to a number. Same
fix: add a `KnownNat m` constraint to the instance.

```
src/Reshape/Flatten.hs:53:19-25: error:
    • Overlapping instances for Flattenable (n - 1) m
        arising from a use of ‘flatten’
      Matching instance:
        instance [overlap ok] (1 <= n, (m + ((n - 1) * m)) ~ (n * m)) =>
                              Flattenable n m
          -- Defined at src/Reshape/Flatten.hs:36:3
      Potentially matching instance:
        instance Flattenable 1 m -- Defined at src/Reshape/Flatten.hs:29:10
      (The choice depends on the instantiation of ‘n, m’
       and the result of evaluating ‘-’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
    • In the second argument of ‘(#)’, namely ‘flatten rest’
      In the expression: unrow r # flatten rest
      In the expression:
        let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
        in unrow r # flatten rest
   |
53 |      in unrow r # flatten rest
   |                   ^^^^^^^
```

Here, the compiler is saying that it doesn't know that `m2 :: L (n-1) m` will
match an instance of `Flattenable` since it might have more than one row. The
fix is to add a constraint for `Flattenable (n-1) m`. Adding this constraint
also means that we can remove the `{-# OVERLAPS #=}` pragma since the instance
won't match with single-row matrices anymore.

After adding the new constraints, our instance now looks like this:

```haskell
instance
  ( 1 <= n,
    m + ((n - 1) * m) ~ n * m,
    KnownNat n,
    KnownNat m,
    Flattenable (n - 1) m
  ) =>
  Flattenable n m where

  flatten mat =
    let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
     in unrow r # flatten rest
```

Compiling our code now returns the following error:

```
src/Reshape/Flatten.hs:53:17: error:
    • Could not deduce (KnownNat ((n - 1) * m))
        arising from a use of ‘#’
      from the context: (1 <= n, (m + ((n - 1) * m)) ~ (n * m),
                         KnownNat n, KnownNat m, Flattenable (n - 1) m)
        bound by the instance declaration
        at src/Reshape/Flatten.hs:(36,3)-(49,17)
    • In the expression: unrow r # flatten rest
      In the expression:
        let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
        in unrow r # flatten rest
      In an equation for ‘flatten’:
          flatten mat = let (r, rest) = ... in unrow r # flatten rest
   |
53 |      in unrow r # flatten rest
   |                 ^
```

Now the compiler is telling us that it is not smart enough to recognise that
if `n` and `m` are `KnownNat`s then so is `(n-1) * m`. Let's
add that as yet another constraint:

```haskell
instance
  ( 1 <= n,
    m + ((n - 1) * m) ~ n * m,
    KnownNat n,
    KnownNat m,
    Flattenable (n - 1) m,
    KnownNat ((n - 1) * m)
  ) =>
  Flattenable n m where

  flatten mat =
    let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
     in unrow r # flatten rest
```

And hooray, our code compiles! At last the mighty compiler has been appeased
and we have successfully defined the row-wise `flatten` function for all
matrices in a type-safe way.

# The `unflatten` function

Now that we've implemented `flatten` we can extend the `Flattenable` type class
to include `unflatten` as well:

```haskell
class Flattenable n m where
  flatten :: L n m -> R (n * m)
  unflatten :: R (n * m) -> L n m
```

To recap, `unflatten` is a function that takes a vector and transforms it into a
matrix. This operation can also be seen as a fold where we recursively take the
first M elements from the input vector and stack them on top of the matrix
constructed by `unflatten`ing the rest of the vector.

Like before, the instance for the single-row matrices is straightforward: all we
need to do is invoke the `row :: R n -> L 1 n` function from the Hmatrix Static
API:

```haskell
instance Flattenable 1 m where
  flatten = unrow
  unflatten = row
```

Also like before, the instance for multi-row matrices is more compliced and
involves recursively calling the `unflatten` function. The body of the function
will look as follows, where `split` and `(===)` are functions to split vectors
and stack matrices, respectively:

```haskell
unflatten vec =
    let (r, rest) = split vec :: (R m, R ((n - 1) T.* m))
     in row r === unflatten rest
```

We will also need to provide several more contraints to the instance. These were
implemented following the same process as for `flatten`: compile, get an error,
add a constraint to fix the error, repeat.

The final code for the multi-row instance ended up looking like this:

```haskell
instance
  ( 1 <= n,
    m + ((n - 1) * m) ~ n * m,
    KnownNat n,
    KnownNat m,
    Flattenable (n - 1) m,
    KnownNat ((n - 1) * m),
    -- New constraints:
    m <= n * m,
    (n * m) - m ~ m * (n - 1),
    m * (n - 1) ~ (n - 1) * m,
    1 + (n - 1) ~ n,
    KnownNat (n * m),
    KnownNat (n - 1)
  ) =>
  Flattenable n m where

  flatten mat =
    let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
     in unrow r # flatten rest

  unflatten vec =
    let (r, rest) = split vec :: (R m, R ((n - 1) T.* m))
     in row r === unflatten rest
```


# A type-safe `reshape` function

Finally, we have all the pieces to implement our type-safe `reshape` function!
Here it is in all its glory:

```haskell
reshape ::
    ( Flattenable n m
    , Flattenable p q
    , n * m ~ p * q
    ) => L n m -> L p q
reshape = unflatten . flatten
```

Our previous work on the `Flattenable` type class has paid off. To reshape a
matrix, all we need to do is `flatten` and `unflatten` our input into the
correct shape, and the compiler take care of the rest.

Here's the final code for everything we've covered in this post:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import Numeric.LinearAlgebra.Static

class Flattenable n m where
  flatten :: L n m -> R (n * m)
  unflatten :: R (n * m) -> L n m

instance Flattenable 1 m where
  flatten = unrow
  unflatten = row

instance
  ( 1 <= n,
    m + ((n - 1) * m) ~ n * m,
    KnownNat n,
    KnownNat m,
    Flattenable (n - 1) m,
    KnownNat ((n - 1) * m),
    m <= n * m,
    (n * m) - m ~ m * (n - 1),
    m * (n - 1) ~ (n - 1) * m,
    1 + (n - 1) ~ n,
    KnownNat (n * m),
    KnownNat (n - 1)
  ) =>
  Flattenable n m where

  flatten mat =
    let (r, rest) = splitRows mat :: (L 1 m, L (n - 1) m)
     in unrow r # flatten rest

  unflatten vec =
    let (r, rest) = split vec :: (R m, R ((n - 1) T.* m))
     in row r === unflatten rest

reshape ::
  ( Flattenable n m,
    Flattenable p q,
    n * m ~ p * q
  ) => L n m -> L p q
reshape = unflatten . flatten
```
