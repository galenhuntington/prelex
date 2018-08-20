Note:  This package requires GHC 8.6 or later.  It also requires
installation of [hpre](https://github.com/galenhuntington/hpre).
Install with `cabal install`.

##  Intro

`prelex` is a library I wrote to solve a simple but recurrent problem.
I separated it into its own package for general use.

You may have noticed that alphabetical order does not match numerical
order, often annoyingly:

```yaml
directory:
  version-10
  version-11
  version-7
  version-8
  version-9
```

In many cases I'd prefer numbers inside strings that are
lexicographically ordered.  This can be accomplished if the numbers
do not need to be sequential.

Example:

```haskell
Prelude Prelex> let zero = 0 :: PrelexDecimal Int
Prelude Prelex> take 20 [zero ..]
[0,10,11,12,13,14,15,16,17,18,19,200,201,202,203,204,205,206,207,208]
Prelude Prelex> succ $ succ $ succ zero
12
```

The basic design is simple: the first digit specifies the number of
digits that come after.  This gives you a stream of numbers increasing
both in value and alphabetically.

The `Int` specifies the underlying type, which determines the
representation.  For example, use `Integer` to get an unbounded type.
The internal value is an ordinary sequential number, and you can
translate back and forth:


```haskell
Prelude Prelex> let val = [0..] !! 100 :: PrelexDecimal Integer
Prelude Prelex> val
289
Prelude Prelex> fromIntegral val :: Integer
100
Prelude Prelex> fromIntegral (succ val) :: Integer
101
```

Numeric literals in Haskell are implicitly cast with `fromIntegral`.
`read` can be used to parse a string.  Use with `Num` instances can
be convenient:

```haskell
Prelude Prelex> 10000 :: PrelexDecimal Integer
48889
Prelude Prelex> read "512345" :: PrelexDecimal Integer
512345
Prelude Prelex> val + 5
294
Prelude Prelex> val + 100
3089
```

With `DataKinds`, you can use prelex in any base from 2 to 36:

```haskell
Prelude Prelex> [1..32] :: [Prelex 5 Int]
[10,11,12,13,14,200,201,202,203,204,210,211,212,213,214,220,221,222,223,224,230,231,232,233,234,240,241,242,243,244,3000,3001]
Prelude Prelex> 1000 :: Prelex 16 Int
32d7
```

Indeed, `type PrelexDecimal = Prelex 10`.

For small bases, or large numbers, the first digit might overflow.
If the first digit is the largest possible (e.g., 9 for base 10),
then the second digit is consulted, and so on.  In this way, there
can be an infinity of values:

```haskell
Prelude Prelex> 10000 :: Prelex 6 Integer
51003033
Prelude Prelex> 10^20 :: PrelexDecimal Integer
99288888888888888888889
Prelude Prelex> 40 :: Prelex 2 Integer
11111001001
Prelude Prelex> 10^120 :: Prelex 36 Integer
zz80fkodb6763nsan6z9njhtwvpvcrjikx3mwkq7m1yxwomsbl1nc2xq6juh4o65tcwh7ps3rb0uu6t8r
```

`Show` and `Read` instances are particularly intended to be used with
`deriving via`:

```haskell
{-# LANGUAGE DataKinds, DerivingVia, GeneralizedNewtypeDeriving #-}
import GHC.Word
import Prelex
newtype BatchId = BatchId Word64
   deriving newtype (Eq, Ord, Enum)
   deriving (Show, Read) via Prelex 16 Word64
```

Finally, the `IsPrelex` class provides `prelexBase`, which takes a
`Proxy` for a prelex type and returns its base:

```haskell
*Main Data.Proxy> prelexBase (Proxy :: Proxy (PrelexDecimal Int))
10
*Main Data.Proxy> :set -XStandaloneDeriving -XDerivingVia -XDataKinds -XTypeApplications
*Main Data.Proxy> deriving via Prelex 16 Word64 instance IsPrelex BatchId
*Main Data.Proxy> prelexBase $ Proxy @BatchId
16
```


##  Design

There are several points in the design space, depending on what
criteria are sought.

For instance, if the numbers don't need to be numerically ordered,
we could have the first digit be only a _maximum_ length, so that we
could have 19, 2, 20, 200, 21, 210, etc.  Even if we do want numerical
order, we could include 20 on that list.

An advantage of prelex, in addition to preserving order, is that
it is a “prefix code”, in that the parser knows when to stop.
This is sometimes useful.  Initially, I had a mere ”lex” ordering,
with a “prelex” variant that adds this property.  But I decided
there was little advantage in packing in a few extra codes, and a
plethora of competing standards was undesirable.  By fixing on prelex,
we get all these nice properties.

For first-digit overflow, 90- means a nine-digit number follows, 91-
means ten digits, and so on.  I considered using a prelex number after
the 9, for a recursive definition, but that would only be helpful if
we're dealing with exponentially huge numbers (or doubly exponential),
which is not the expected use case.  In the expected case, it is bloat.

There are other variants that make some codes slightly shorter
for certain usage profiles, but at the expense of introducing a
proliferation of types.
