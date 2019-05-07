module BSimplified.QMM (
    pdnfTable,
    hammingGroup,
    pickHammingGroup
) where

import BSimplified.Bits

import Data.Bits (Bits (..))
import Data.Tuple.Extra (both, first, second, dupe)
import Data.List (nub, partition, unfoldr)
import Data.Maybe (fromJust)

data TruthTableRow i = TruthTableRow { var :: i, dontcares :: i } deriving Eq

instance (Show i, Integral i) => Show (TruthTableRow i) where
    show = uncurry (flip (.) (++ " }") . ((++) . ((++) "TruthTableRow { " . (++ ", ")))). first ("var = " ++) . second ("dontcares = " ++) . both rawBitsToStr . first var . second dontcares . dupe

type TruthTable i = [TruthTableRow i]

pdnfTable :: QMyBits q => [q] -> [q] -> Maybe (TruthTable Integer)
pdnfTable tb []
    | all isValid tb = Just $ map (flip TruthTableRow 0 . (fromJust . toRawBits)) tb  
    | otherwise = Nothing
pdnfTable tb en
    | all isValid tb && all isValid en && length tb == length en = Just $ zipWith (flip (.) (fromJust . toRawBits) . (TruthTableRow . (fromJust . toRawBits))) tb en 
    | otherwise = Nothing

type Group i = (TruthTableRow i, TruthTableRow i)


hammingTT :: TruthTableRow Integer -> TruthTableRow Integer -> Maybe Int
hammingTT (TruthTableRow lvar ld) (TruthTableRow rvar rd) = RawBits (lvar .|. ld) `hamming` RawBits (rvar .|. rd)

hammingGroup :: TruthTable Integer -> ([Group Integer], [TruthTableRow Integer])
hammingGroup vals = (left, [x | x <- nub $ concatMap snd l, all (uncurry (&&) . both (x/=)) left])
    where
        l = unfoldr (\xs -> if null xs then Nothing else let (h:tl) = xs in Just (first (map ((,) h)) $ partition ((== Just 1) . hammingTT h) xs, tl)) vals
        left = concatMap fst l


pickHammingGroup :: QMyBits a => a -> ([Group Integer], [TruthTableRow Integer]) -> [Group Integer]
pickHammingGroup b = filter (uncurry (||) . both ((==) (fromJust (toRawBits b)) . var)) . fst
