{-# LANGUAGE Haskell2010, ScopedTypeVariables, DataKinds #-}

module Props where

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
import Data.Bits
import Data.Proxy
import Data.Word
import Data.Word.Odd
import GHC.TypeLits

import Equiv

genOddWord :: forall a.
    (Integral a, Bounded a) => Proxy a -> Gen a
genOddWord _ =
    fmap (fromIntegral :: Integer -> a) $ choose (
        fromIntegral (minBound::a),
        fromIntegral (maxBound::a))

propRotateRL :: (Integral a, Bounded a, FiniteBits a) =>
    Proxy a -> Property
propRotateRL proxy =
    property $ do
        x <- genOddWord proxy
        i <- choose (0, 2*finiteBitSize x)
        return $ (==) x $ flip rotateL i $ rotateR x i

propInBounds :: forall n a.
    (KnownNat n, Integral a, Bounded a, Enum a, FiniteBits a, Read a, Show a) =>
    Proxy n -> Proxy a -> Property
propInBounds _ _ = property $ \(us :: [UFunc n]) ->
    let tstFn = foldr (.) id $ map fromUFunc us :: a -> a
        value = toInteger $ tstFn 0
    in value >= toInteger (minBound :: a) &&
       value <= toInteger (maxBound :: a)
