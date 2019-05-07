{-# LANGUAGE BangPatterns, TupleSections #-}
module BSimplified.QMM (
    rawBitsList,
    strBitsList,
    TruthTableRow (..),
    CompressTree (..),
    pdnfForest,
    quineMcCluskey,
    traceQuineMcCluskey,
    essentialPrimeImplicants,
    bitStr,
    minTerms,
    traceMinTerms,
    minTermsStr,
    traceMinTermsStr
) where

import BSimplified.Bits
import BSimplified.Utils

import Data.Tuple.Extra (first, second, dupe)
import Data.Bits (Bits (..))
import Data.List (nub, unfoldr, intersect, elemIndices)
import Data.Maybe (fromJust, isJust)
import Data.Either (Either (..), partitionEithers)
import Data.Hash (Hash (..), hashInt)
import Data.Word (Word64)
import qualified Data.Set as S
import Control.Monad (unless)

type Identifier = Word64

data TruthTableRow i = TruthTableRow { var :: i, dontcare :: Bool } deriving Eq
data CompressTree i = CTEmpty | CTNode { row :: TruthTableRow i, mergedFlag :: Int, identifier :: Identifier, prevLeft :: CompressTree i, prevRight :: CompressTree i } deriving Show

instance (Show i, Integral i) => Show (TruthTableRow i) where
    show (TruthTableRow x y) = "TruthTableRow { var = " ++ rawBitsToStr x ++ " (binary), dontcare = " ++ show y ++ " }"

instance Eq i => Eq (CompressTree i) where
    (==) CTEmpty CTEmpty = True
    (==) x y = identifier x == identifier y

instance Ord i => Ord (CompressTree i) where
    compare = flip (.) identifier . (compare . identifier)

roots :: (CompressTree i -> a) -> CompressTree i -> [a]
roots _ CTEmpty = []
roots f c@(CTNode _ _ _ CTEmpty CTEmpty) = [f c]
roots f (CTNode _ _ _ pl pr) = roots f pl ++ roots f pr

{-# INLINE tRw #-}
tRw :: QMyBits p => p -> Bool -> TruthTableRow Integer
tRw = TruthTableRow . (fromJust . toRawBits)

{-# INLINE rawBitsList #-}
rawBitsList :: [Integer] -> [RawBits]
rawBitsList = map RawBits

{-# INLINE strBitsList #-}
strBitsList :: [String] -> Maybe [RawBits]
strBitsList xs 
    | all isJust m = Just $ map (RawBits . fromJust) m
    | otherwise = Nothing
    where
        m = map ((toRawBits :: StrBits -> Maybe Integer) . StrBits) xs

{-# INLINE viewTRw #-}
viewTRw :: TruthTableRow Integer -> RawBits
viewTRw = RawBits . var

oneHammingFlag :: TruthTableRow Integer -> TruthTableRow Integer -> Int
oneHammingFlag x y = go 0 (fromJust $ toRawBits $ viewTRw x) (fromJust $ toRawBits $ viewTRw y)
    where
        go :: Int -> Integer -> Integer -> Int
        go !i !l !r
            | l == 0 && l == r || l .&. 1 /= r .&. 1 = i
            | otherwise = go (succ i) (l `shiftR` 1) (r `shiftR` 1)

bitStr :: CompressTree Integer -> String
bitStr (CTNode r _ _ CTEmpty CTEmpty) = show $ var r
bitStr (CTNode _ mf _ (CTNode r1 _ _ CTEmpty CTEmpty) (CTNode r2 _ _ CTEmpty CTEmpty)) = let f = reverse . show . RawBits . var; fr1 = f r1; fr2 = f r2 in
    reverse $ replaceAt mf '-' $ if length fr1 < length fr2 then fr2 else fr1 
bitStr (CTNode _ mf _ pl pr) 
    | l == r || length l > length r = l
    | length l < length r = r 
    | otherwise = replaceAt (head (diffIndices l r)) '-' l
    where
        f = reverse . replaceAtIgnored mf '-' '-' . reverse . bitStr
        l = f pl 
        r = f pr 

{-# INLINE isSameMergedFlag #-}
isSameMergedFlag :: CompressTree Integer -> CompressTree Integer -> Bool
isSameMergedFlag CTEmpty _ = False
isSameMergedFlag _ CTEmpty = False
isSameMergedFlag x y 
    | length xb < length yb = f (zero ++ xb) yb
    | length xb > length yb = f xb (zero ++ yb) 
    | otherwise = f xb yb
    where
        xb = bitStr x
        yb = bitStr y
        zero = replicate (abs (length xb - length yb)) '0'
        f = let g = elemIndices '-' in flip (.) g . ((==) . g)

{-# INLINE isSameIdentifier #-}
isSameIdentifier :: CompressTree i -> CompressTree i -> Bool
isSameIdentifier CTEmpty _ = False
isSameIdentifier _ CTEmpty = False
isSameIdentifier x y = let rx = roots identifier x in length (rx `intersect` roots identifier y) == length rx

treeFromPair :: CompressTree Integer -> CompressTree Integer -> Maybe (CompressTree Integer)
treeFromPair t1@(CTNode r1 _ i1 _ _) t2@(CTNode r2 _ i2 _ _)
    | viewTRw r1 `hamming` viewTRw r2 == Just 1 && isSameMergedFlag t1 t2 = flip fmap (viewTRw r1 `rmHammingDiff` viewTRw r2) $ \(RawBits x) -> 
        CTNode (TruthTableRow x False) (oneHammingFlag r1 r2) (i1 `xor` i2) t1 t2
    | otherwise = Nothing
treeFromPair _ _ = Nothing

{-# INLINE tryTreeFromPair #-}
tryTreeFromPair :: CompressTree Integer -> CompressTree Integer -> Either (CompressTree Integer, CompressTree Integer) (CompressTree Integer)
tryTreeFromPair t1 t2 = maybe (Left (t1, t2)) Right $ treeFromPair t1 t2

{-# INLINE tableRow #-}
tableRow :: QMyBits p => p -> Bool -> Maybe (TruthTableRow Integer)
tableRow b1 b2 | isValid b1 = Just $ tRw b1 b2 | otherwise = Nothing

type Minterms q = [q]
type DontCares = [Bool]
type CompressWoods i = [CompressTree i]

-- | Construct a list of binary tree structures dedicated to performing the Quine-McCluskey method etc. 
-- from a list of minterms represented by instances (bit strings) of `QMyBits`.
pdnfForest :: (Eq p, QMyBits p) => Minterms p -> DontCares -> Maybe (CompressWoods Integer)
pdnfForest v dc 
    | all isValid v = let nv = nub v in pdnfForest' nv dc $ map (asWord64 . hashInt) $ take (length nv) $ iterate (+1) 0
    | otherwise = Nothing
    where
        pdnfForest' vv [] hs = Just $ zipWith (\x i -> CTNode (fromJust $ tableRow x False) (-1) i CTEmpty CTEmpty) vv hs
        pdnfForest' vv ddc hs
            | length vv == length ddc = Just $ zipWith3 (\x y z -> CTNode (fromJust $ tableRow x y) (-1) z CTEmpty CTEmpty) vv ddc hs
            | otherwise = Nothing

compress :: CompressWoods Integer -> (CompressWoods Integer, CompressWoods Integer) 
compress tr = (nub $ filter (not . isRedudant) left1Only ++ filter (not . isRedudant) left2Only, rightOnly)
    where
        redudant = partitionEithers $ nub $ concat $ unfoldr (\xs -> if null xs then Nothing else let (h:ts) = xs in Just (map (tryTreeFromPair h) xs, ts)) tr
        left1Only = nub $ map fst $ fst redudant
        left2Only = nub $ map snd $ fst redudant
        rightOnly = snd redudant
        isRedudant = flip any rightOnly . isSameIdentifier

{-# INLINE traceWoods #-}
traceWoods :: [(Identifier, String)] -> CompressWoods i -> [String]
traceWoods valmap = map (unwords . map (fromJust . flip lookup valmap) . roots identifier)

newtype PrimeImplicants i = PrimeImplicants (CompressWoods i) deriving Show

{-# INLINE toCw #-}
toCw :: PrimeImplicants i -> CompressWoods i
toCw (PrimeImplicants cw) = cw

-- | Take prime implicants.
quineMcCluskey :: CompressWoods Integer -> PrimeImplicants Integer
quineMcCluskey = PrimeImplicants . concat . qm . compress
    where
        qm (p, []) = [p]
        qm (p, y) = p : qm (compress y)

-- | Take prime implicants and print out the state of compressing transition.
traceQuineMcCluskey :: CompressWoods Integer -> [String] -> IO (Maybe (PrimeImplicants Integer))
traceQuineMcCluskey x vm 
    | length x == length vm = Just . PrimeImplicants . concat <$> qm 1 (compress x)
    | otherwise = return Nothing
   where
        valmap = zip (map identifier x) vm
        qm i (p, y) 
            | null y = [p] <$ f valmap p
            | otherwise = f valmap p >> putStrLn ("The state of compression #" ++ show i ++ ": " ++ show (traceWoods valmap y)) >> ((p :) <$> qm (succ i) (compress y))
            where
                f vp q = unless (null q) $ putStrLn ("Found prime implicants: " ++ show (traceWoods vp p))

{-# INLINE minterm #-}
minterm :: CompressWoods Integer -> CompressWoods Integer
minterm = filter ((==False) . dontcare . row)

type PDNF = [CompressTree Integer]
type EssentialPrimeImplicants i = PrimeImplicants i

-- | Take essential prime implicants.
essentialPrimeImplicants :: PDNF -> PrimeImplicants Integer -> EssentialPrimeImplicants Integer
essentialPrimeImplicants tr pim = PrimeImplicants $ foldr (\x acc -> maybe acc (: acc) (findEssential [] x (toCw pim))) [] $ minterm tr
    where
        findEssential rx x (m:ms)
            | identifier x `elem` roots identifier m = if null rx then findEssential [m] x ms else Nothing
            | otherwise = findEssential rx x ms
        findEssential [e] _ [] = Just e
        findEssential _ _ _ = Nothing

subSameTerms :: Ord a => [S.Set a] -> [Either (S.Set a) (S.Set a, S.Set a)]
subSameTerms = unfoldr (\xs -> if null xs then Nothing else let (y:ys) = xs in Just $ either ((, ys) . Left) (first Right) $ findTerm y ys)
    where
        findTerm :: Ord a => S.Set a -> [S.Set a] -> Either (S.Set a) ((S.Set a, S.Set a), [S.Set a])
        findTerm e [] = Left e
        findTerm e ys = maybe (Left e) (\(t, ts) -> Right ((e, S.fromList t), map S.fromList ts)) $ findDelete (any (`elem` S.toList e)) $ map S.toList ys

-- | Create most simplified expressions based on the Petrik's method.
minTerms :: CompressWoods Integer -> CompressWoods Integer
minTerms tr = concatMap (S.toList . either id (S.map fst . S.filter (uncurry (==) . first fst . second snd . dupe) . uncurry S.cartesianProduct)) $ subSameTerms petcom
    where
        pim = quineMcCluskey tr
        petcom = [foldr (\p acp -> if identifier m `elem` roots identifier p then S.insert p acp else acp) S.empty (toCw pim) | m <- minterm tr]

traceMinTerms :: CompressWoods Integer -> [String] -> IO (Maybe (CompressWoods Integer))
traceMinTerms tr valnames = fmap (fmap (\x -> concatMap (S.toList . either id (S.map fst . S.filter (uncurry (==) . first fst . second snd . dupe) . uncurry S.cartesianProduct)) $ subSameTerms [foldr (\p acp -> if identifier m `elem` roots identifier p then S.insert p acp else acp) S.empty (toCw x) | m <- minterm tr])) $ traceQuineMcCluskey tr valnames

minTermsStr :: CompressWoods Integer -> [a] -> Maybe [[a]]
minTermsStr tr valnames
    | length tr == length valnames = Just $ map (map (fromJust . flip lookup valmap) . roots identifier) $ minTerms tr
    | otherwise = Nothing
    where
        valmap = zip (map identifier tr) valnames

traceMinTermsStr :: CompressWoods Integer -> [String] -> IO (Maybe [[String]])
traceMinTermsStr tr valnames
    | length tr == length valnames = fmap (map (map (fromJust . flip lookup valmap) . roots identifier)) <$> traceMinTerms tr valnames
    | otherwise = return Nothing
    where
        valmap = zip (map identifier tr) valnames
