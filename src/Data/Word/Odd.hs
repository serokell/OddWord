{-# LANGUAGE Haskell2010, ScopedTypeVariables, CPP,
             DeriveDataTypeable, DataKinds, KindSignatures,
             TypeFamilies, TypeOperators, UndecidableInstances,
             FlexibleInstances #-}

module Data.Word.Odd (
    -- * Odd Word Wrapper
    OddWord,
    WrapWord,

    -- * Type Numbers
    TypeNum,

    -- * Finite Bits
    FiniteBitsBase(
        subWordClz,
        subWordCtz),

    -- * Predefined Odd Words
    Word1, Word2, Word3, Word4, Word5, Word6, Word7,
    Word9,  Word10, Word11, Word12, Word13, Word14, Word15,
    Word17, Word18, Word19, Word20, Word21, Word22, Word23, Word24,
    Word25, Word26, Word27, Word28, Word29, Word30, Word31,
    Word33, Word34, Word35, Word36, Word37, Word38, Word39, Word40,
    Word41, Word42, Word43, Word44, Word45, Word46, Word47, Word48,
    Word49, Word50, Word51, Word52, Word53, Word54, Word55, Word56,
    Word57, Word58, Word59, Word60, Word61, Word62, Word63
) where

import Data.Bits
import Data.Proxy
import Data.Word
import Data.Function
import Data.Typeable
import GHC.TypeLits

#include "MachDeps.h"

-- | 'OddWord' provides a range of unsigned integer word types with a length in
-- bits specified at the type level. It is a type alias around 'WrapWord'
-- which automatically determines a suitable base type.
type OddWord (n::Nat) = WrapWord (BaseWord n) n

type family BaseWordImpl (cmp::Ordering) where
    BaseWordImpl GT = Integer
    BaseWordImpl u  = Word

type BaseWord (n::Nat) = BaseWordImpl (CmpNat n WORD_SIZE_IN_BITS)

-- | 'WrapWord' encapsulates an integer type @a@ and provides a set of
-- mumeric instances which use only the first @n@ bits of that type.
--
-- The behaviour of an 'WrapWord' is undefined if the specified length is
-- greater than that of the underlying integer type. The behaviour is also
-- undefined if the specified length is equal to that of the underlying integer
-- type and that type is also signed.
newtype WrapWord a (n::Nat) = OW {unOW :: a} deriving (Eq, Ord, Typeable)

newtype TypeNumBuilder (a::Nat) = TypeNumBuilder Int

fromTypeNum :: TypeNumBuilder a -> Int
fromTypeNum (TypeNumBuilder x) = x

-- | Intances of 'TypeNum' represent type-level numbers.
class TypeNum (a::Nat) where
    typeNum :: TypeNumBuilder a

-- | Provides a more efficient mechanism for converting 'Nat'-kinded types into
-- small integers than 'KnownNat'.
#if MIN_VERSION_base(4,11,0)
-- Decomposes Nats in log2(n) recursions, one bit at a time.
data ZNat = IsZ | NonZE Nat | NonZO Nat

type family ToZNatImpl (n::Nat) (lsb::Nat) where
    ToZNatImpl 0 0 = IsZ
    ToZNatImpl n 0 = NonZE n
    ToZNatImpl n 1 = NonZO n

type ToZNat n = ToZNatImpl n (Mod n 2)

class ZNatValue (n::ZNat) where
    znatIntVal :: proxy n -> Int

instance ZNatValue IsZ where
    znatIntVal _ = 0
    {-# INLINE znatIntVal #-}

instance ZNatValue (ToZNat (Div n 2)) => ZNatValue (NonZE n) where
    znatIntVal _ = 2 * (znatIntVal (Proxy :: Proxy (ToZNat (Div n 2))))
    {-# INLINE znatIntVal #-}

instance ZNatValue (ToZNat (Div n 2)) => ZNatValue (NonZO n) where
    znatIntVal _ = 1 + 2 * (znatIntVal (Proxy :: Proxy (ToZNat (Div n 2))))
    {-# INLINE znatIntVal #-}
#else
-- For older GHCs that don't support Div and Mod, decomposes Nats in
-- 16*log16(n) recursions for values of n below 2^16.
data ZNat = IsZ | NonZ Nat | NonZ4 Nat | NonZ8 Nat | NonZ12 Nat

-- Regarding u, v, and w, GHC 7.10 doesn't like wildcards in type families.
type family ToZNatImpl
        (n::Nat) (nz4::Ordering) (nz8::Ordering) (nz12::Ordering) where
    ToZNatImpl 0 LT LT LT = IsZ
    ToZNatImpl n LT LT LT = NonZ n
    ToZNatImpl n u  LT LT = NonZ4 n
    ToZNatImpl n u  v  LT = NonZ8 n
    ToZNatImpl n u  v  w  = NonZ12 n

type ToZNat n = ToZNatImpl n (CmpNat n 16) (CmpNat n 256) (CmpNat n 4096)

class ZNatValue (n::ZNat) where
    znatIntVal :: proxy n -> Int

instance ZNatValue IsZ where
    znatIntVal _ = 0
    {-# INLINE znatIntVal #-}

instance ZNatValue (ToZNat (n - 1)) => ZNatValue (NonZ n) where
    znatIntVal _ = 1 + (znatIntVal (Proxy :: Proxy (ToZNat (n - 1))))
    {-# INLINE znatIntVal #-}

instance ZNatValue (ToZNat (n - 16)) => ZNatValue (NonZ4 n) where
    znatIntVal _ = 16 + (znatIntVal (Proxy :: Proxy (ToZNat (n - 16))))
    {-# INLINE znatIntVal #-}

instance ZNatValue (ToZNat (n - 256)) => ZNatValue (NonZ8 n) where
    znatIntVal _ = 256 + (znatIntVal (Proxy :: Proxy (ToZNat (n - 256))))
    {-# INLINE znatIntVal #-}

instance ZNatValue (ToZNat (n - 4096)) => ZNatValue (NonZ12 n) where
    znatIntVal _ = 4096 + (znatIntVal (Proxy :: Proxy (ToZNat (n - 4096))))
    {-# INLINE znatIntVal #-}
#endif

instance (ZNatValue (ToZNat n)) => TypeNum n where
    typeNum = TypeNumBuilder
        (fromIntegral $ znatIntVal (Proxy :: Proxy (ToZNat n)))

-- | Required to implement 'FiniteBits' for an 'WrapWord' based on type @a@.
class Bits a => FiniteBitsBase a where
    -- | Count the leading zeros on a @w@-bit wide word.
    subWordClz :: Int -> a -> Int
    subWordClz w x = (w-1) - worker (w-1)
        where worker i | i < 0       = i
                       | testBit x i = i
                       | otherwise   = worker (i-1)
    -- | Count the trailing zeros on a @w@-bit wide word.
    subWordCtz :: Int -> a -> Int
    subWordCtz w x = worker 0
        where worker i | i >= w      = i
                       | testBit x i = i
                       | otherwise   = worker (i+1)

instance FiniteBitsBase Word8 where
    subWordClz w x = countLeadingZeros x + w - finiteBitSize x
    subWordCtz w x = min (countTrailingZeros x) w

instance FiniteBitsBase Word16 where
    subWordClz w x = countLeadingZeros x + w - finiteBitSize x
    subWordCtz w x = min (countTrailingZeros x) w

instance FiniteBitsBase Word32 where
    subWordClz w x = countLeadingZeros x + w - finiteBitSize x
    subWordCtz w x = min (countTrailingZeros x) w

instance FiniteBitsBase Word64 where
    subWordClz w x = countLeadingZeros x + w - finiteBitSize x
    subWordCtz w x = min (countTrailingZeros x) w

instance FiniteBitsBase Word where
    subWordClz w x = countLeadingZeros x + w - finiteBitSize x
    subWordCtz w x = min (countTrailingZeros x) w

instance FiniteBitsBase Integer where

-- | Wraps both parts of a homogenous pair with the WrapWord constructor.
pairOW :: (a, a) -> (WrapWord a n, WrapWord a n)
pairOW = uncurry ((,) `on` OW)

-- | An WrapWord with all the bits set, used for masking.
owMask :: forall a n. (Num a, Bits a, TypeNum n) => WrapWord a n
owMask = OW . (flip (-) 1) . bit $ fromTypeNum (typeNum :: TypeNumBuilder n)

-- | Smart constructor for WrapWords which masks off the unused upper bits.
maskOW :: forall a n. (Num a, Bits a, TypeNum n) => a -> WrapWord a n
maskOW w = OW $ w .&. unOW (owMask :: WrapWord a n)
{-# INLINE maskOW #-}

-- | Applies a function to the first component of each pair in a list thereof.
mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f xs = map (\(a,c) -> (f a,c)) xs

--
-- Instances for the WrapWord type
--
-- The instances largely forward operations to the underlying integer type
-- while wrapping and unwrapping the newtype, and masking or otherwise
-- adjusting the results as appropriate for the desired bit length of the word.
--

instance (Show a) => Show (WrapWord a n) where
    showsPrec p (OW x) s = showsPrec p x s
    show (OW x)          = show x
    showList xs          = showList $ map unOW xs

instance (Read a, Num a, Bits a, TypeNum n) => Read (WrapWord a n) where
    readsPrec p s = mapFst maskOW $ readsPrec p s
    readList s    = mapFst (map maskOW) $ readList s

instance (Num a, Bits a, TypeNum n) => Num (WrapWord a n) where
    (OW l) + (OW r) = maskOW $ (l + r)
    (OW l) * (OW r) = maskOW $ (l * r)
    (OW l) - (OW r) = maskOW $ (l - r)
    negate (OW x)   = maskOW $ negate x
    abs w = w
    signum (OW x) | x == 0    = 0
                  | otherwise = 1
    fromInteger i = maskOW $ fromInteger i

instance (Real a, Bits a, TypeNum n) => Real (WrapWord a n) where
    toRational (OW x) = toRational x

instance (Num a, Bits a, TypeNum n) => Bounded (WrapWord a n) where
    minBound = 0
    maxBound = owMask

instance (Enum a, Ord a, Num a, Bits a, TypeNum n) => Enum (WrapWord a n) where
    succ x = x + 1
    pred x = x - 1
    toEnum i | i >= 0 && fromIntegral i <= unOW (owMask :: WrapWord a n)
             = OW $ toEnum i
             | otherwise = error "WrapWord: toEnum: Index out of bounds."
    fromEnum (OW x) = fromEnum x
    enumFrom x = enumFromTo x owMask
    enumFromThen x1 x2 = enumFromThenTo x1 x2 bound
                         where bound | x2 >= x1 = owMask
                                     | otherwise = 0
    enumFromTo (OW x) (OW y) = map OW $ enumFromTo x y
    enumFromThenTo (OW x1) (OW x2) (OW y) = map OW $ enumFromThenTo x1 x2 y

instance (Integral a, Bits a, TypeNum n) => Integral (WrapWord a n) where
    quot (OW n) (OW d) = OW $ quot n d
    rem  (OW n) (OW d) = OW $ rem n d
    div  (OW n) (OW d) = OW $ div n d
    mod  (OW n) (OW d) = OW $ mod n d
    quotRem (OW n) (OW d) = pairOW $ quotRem n d
    divMod  (OW n) (OW d) = pairOW $ divMod n d
    toInteger (OW x) = toInteger x

instance (Num a, Bits a, TypeNum n) => Bits (WrapWord a n) where
    (OW l) .&. (OW r) = OW $ l .&. r
    (OW l) .|. (OW r) = OW $ l .|. r
    xor (OW l) (OW r) = OW $ xor l r
    complement x = x `xor` owMask
    bit n | n < fromTypeNum (typeNum :: TypeNumBuilder n)
          = OW $ bit n
          | otherwise = OW 0
    setBit (OW x) n | n < fromTypeNum (typeNum :: TypeNumBuilder n)
                    = OW $ setBit x n
                    | otherwise = OW x
    clearBit (OW x) n = OW $ clearBit x n
    complementBit (OW x) n | n < fromTypeNum (typeNum :: TypeNumBuilder n)
                           = OW $ complementBit x n
                           | otherwise = OW x
    testBit (OW x) n = testBit x n
    bitSize _ = fromTypeNum (typeNum :: TypeNumBuilder n)
    bitSizeMaybe _ = Just $ fromTypeNum (typeNum :: TypeNumBuilder n)
    isSigned _ = False
    shiftL (OW x) n = maskOW $ shiftL x n
    shiftR (OW x) n = OW $ shiftR x n
    rotateL (OW x) n = OW $
        (shiftL x n' .&. unOW (owMask :: WrapWord a n)) .|. shiftR x (w-n')
        where n' = n `mod` w
              w = fromTypeNum (typeNum :: TypeNumBuilder n)
    rotateR (OW x) n = OW $
        shiftR x n' .|. (shiftL x (w-n') .&. unOW (owMask :: WrapWord a n))
        where n' = n `mod` w
              w  = fromTypeNum (typeNum :: TypeNumBuilder n)
    popCount (OW x) = popCount x

instance (Num a, FiniteBitsBase a, TypeNum n) => FiniteBits (WrapWord a n) where
    finiteBitSize _ = fromTypeNum (typeNum :: TypeNumBuilder n)
    countLeadingZeros (OW x) =
        subWordClz (fromTypeNum (typeNum :: TypeNumBuilder n)) x
    countTrailingZeros (OW x) =
        subWordCtz (fromTypeNum (typeNum :: TypeNumBuilder n)) x

--
-- Predefined Odd Words
--

type Word1  = OddWord 1
type Word2  = OddWord 2
type Word3  = OddWord 3
type Word4  = OddWord 4
type Word5  = OddWord 5
type Word6  = OddWord 6
type Word7  = OddWord 7
--type Word8
type Word9  = OddWord 9
type Word10 = OddWord 10
type Word11 = OddWord 11
type Word12 = OddWord 12
type Word13 = OddWord 13
type Word14 = OddWord 14
type Word15 = OddWord 15
--type Word16
type Word17 = OddWord 17
type Word18 = OddWord 18
type Word19 = OddWord 19
type Word20 = OddWord 20
type Word21 = OddWord 21
type Word22 = OddWord 22
type Word23 = OddWord 23
type Word24 = OddWord 24
type Word25 = OddWord 25
type Word26 = OddWord 26
type Word27 = OddWord 27
type Word28 = OddWord 28
type Word29 = OddWord 29
type Word30 = OddWord 30
type Word31 = OddWord 31
--type Word32
type Word33 = OddWord 33
type Word34 = OddWord 34
type Word35 = OddWord 35
type Word36 = OddWord 36
type Word37 = OddWord 37
type Word38 = OddWord 38
type Word39 = OddWord 39
type Word40 = OddWord 40
type Word41 = OddWord 41
type Word42 = OddWord 42
type Word43 = OddWord 43
type Word44 = OddWord 44
type Word45 = OddWord 45
type Word46 = OddWord 46
type Word47 = OddWord 47
type Word48 = OddWord 48
type Word49 = OddWord 49
type Word50 = OddWord 50
type Word51 = OddWord 51
type Word52 = OddWord 52
type Word53 = OddWord 53
type Word54 = OddWord 54
type Word55 = OddWord 55
type Word56 = OddWord 56
type Word57 = OddWord 57
type Word58 = OddWord 58
type Word59 = OddWord 59
type Word60 = OddWord 60
type Word61 = OddWord 61
type Word62 = OddWord 62
type Word63 = OddWord 63
