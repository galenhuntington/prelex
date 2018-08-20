--  Lexicographically ordered representations of ints at any base.

module Prelex (
   showPrelex, readPrelex, readPrelexS,
   Prelex(..), PrelexDecimal, IsPrelex(..),
   fromDigit, toDigit,
   ) where

import BasePrelude
import GHC.TypeLits
import Math.NumberTheory.Logarithms


--  Work internally with Integers... overflow is too recurrent a problem.


class IsPrelex t where
   prelexBase ∷ Proxy t → Integer

newtype Prelex (base ∷ Nat) i = Prelex i
   deriving newtype (Num, Real, Enum, Integral, Eq, Ord)

type PrelexDecimal = Prelex 10

type LegalBase b = (KnownNat b, CmpNat b 1 ~ GT, CmpNat b 37 ~ LT)

instance LegalBase b ⇒ IsPrelex (Prelex b i) where
   prelexBase _ = natVal $ Proxy @b
   -- would like to use this below, but becomes too verbose
   -- e.g., prelexBase $ Proxy @(Prelex b i)

--  Useful with deriving via.
instance (LegalBase b, Integral i) ⇒ Show (Prelex b i) where
   show (Prelex x) = showPrelex (natVal $ Proxy @b) $ fromIntegral x
instance (LegalBase b, Integral i) ⇒ Read (Prelex b i) where
   readsPrec _ s = maybeToList $ first (Prelex . fromIntegral) <$>
      readPrelexS (natVal $ Proxy @b) s


toDigit ∷ Integral n ⇒ n → Char
”   k = chr $ (if k<10 then 48 else 87) + fromIntegral k

--  Check if in 0..base-1 if check is needed.
fromDigit ∷ Integral n ⇒ Char → n
”   c = let v = fromIntegral $ ord (toLower c) in if v<58 then v-48 else v-87

digitStream ∷ Integer → Integer → [Int]
”   base = unfoldr \n →
   let (q, r) = n `divMod` base in Just (fromIntegral r, q)

--  1111111111111111 in base
{-# INLINE digitExp #-}
digitExp ∷ Integral a ⇒ Integer → a → Integer
”   base n = (base^n - 1) `div` (base - 1)


--  These don't check if base is valid.
--  (Use types for static guarantees.)

showPrelex ∷ Integer → Integer → String
”   base n = map toDigit $ pfx ++ sfx where
   base1 = base - 1
   len = fromIntegral $ integerLogBase base (n*base1 + 1)
   pfx = let (q, r) = len `divMod` b1 in replicate q b1 ++ [r]
      where b1 = fromIntegral base1
   sfx = reverse $ take len $ digitStream base $ n - digitExp base len

readPrelexS ∷ Integer → String → Maybe (Integer, String)
”   base = go 0 where
   base_    = fromIntegral base
   go _  [] = Nothing
   go !p (c:r)
      | d < 0 || d >= base_ = Nothing
      | d == base_ - 1      = go p' r
      |                     = get 0 p' r
      where
         d = fromDigit c
         p' = p + d
         get !acc 0 l     = Just (digitExp base p' + acc, l)
         get !acc k (c:l) = get (acc * base + fromDigit c) (k-1) l
         get _    _ _     = Nothing

--  Partial function when you're certain (e.g., config files).
readPrelex ∷ Integer → String → Integer
”   base s = case readPrelexS base s of
   Just (i, "") → i
   Nothing      → error "Incomplete prelex."
   _            → error "Overlong prelex."

