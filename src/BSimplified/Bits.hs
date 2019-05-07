module BSimplified.Bits (
    rawBitsToStr,
    QMyBits (..),
    StrBits (..),
    DigitListBits (..),
    RawBits (..),
    hamming,
    rmHammingDiff
) where

import Data.Bits (Bits (..), bitSizeMaybe)
import Data.Bool (bool)
import Data.Char (digitToInt, intToDigit)
import Data.List (unfoldr, findIndices)
import Data.Tuple.Extra (first, second, dupe)
import Data.Maybe (fromJust, isJust)

{-# INLINE rawHamming #-}
rawHamming :: Bits a => a -> a -> Int
rawHamming = (.) popCount . xor

bitsToDigitList :: Integral i => i -> [i]
bitsToDigitList = reverse . unfoldr (\x -> if x > 0 then Just (uncurry (flip (,)) $ divMod x 2) else Nothing)

rawBitsToStr :: (Integral i, Show i) => i -> String
rawBitsToStr xs = let l = filter (/=',') $ tail $ init $ show $ bitsToDigitList xs in if null l then "0" else l

class Eq a => QMyBits a where
    isValid :: a -> Bool
    toRawBits :: (Num i, Bits i) => a -> Maybe i

newtype StrBits = StrBits String deriving Eq
newtype DigitListBits = DigitListBits [Int] deriving Eq
newtype RawBits = RawBits Integer deriving Eq

instance QMyBits StrBits where
    isValid (StrBits s) = isValid $ DigitListBits $ map digitToInt s
    toRawBits (StrBits s) = toRawBits $ DigitListBits $ map digitToInt s

instance Show StrBits where
    show (StrBits s) = s

instance QMyBits DigitListBits where
    isValid (DigitListBits xs) = all (uncurry (||) . first (==1) . second (==0) . dupe) xs
    toRawBits ss@(DigitListBits xs)
        | isValid ss = Just $ sum $ zipWith (*) (reverse $ take (length xs) $ iterate (*2) 1) (map fromIntegral xs)
        | otherwise = Nothing

instance Show DigitListBits where
    show (DigitListBits s) = map intToDigit s

instance QMyBits RawBits where
    isValid _ = True
    toRawBits (RawBits x) = Just $ fromIntegral x

instance Show RawBits where
    show (RawBits val) = rawBitsToStr val

hamming :: (QMyBits a, QMyBits b) => a -> b -> Maybe Int
hamming x y 
    | isValid x && isValid y = Just $ rawHamming (fromJust $ toRawBits x) (fromJust (toRawBits y :: Maybe Integer))
    | otherwise = Nothing

rmHammingDiff :: (Show a, QMyBits a, Show b, QMyBits b) => a -> b -> Maybe RawBits
rmHammingDiff x y
    | isValid x && isValid y = let sx = show x; sy = show y in 
        if length sx < length sy then f (replicate (length sy - length sx) '0' ++ sx) sy else if length sx > length sy then f sx (replicate (length sx - length sy) '0' ++ sy) else f sx sy
    | otherwise = Nothing
    where
        f l r = fmap RawBits $ toRawBits $ StrBits $ concat $ zipWith (\l r -> if l /= r then [] else [l]) l r
