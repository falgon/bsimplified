module BSimplified.Parse (
    allPattern
) where

import BSimplified.Tokenize
import BSimplified.Utils

import Data.Tuple.Extra (first, second, fst3, thd3, dupe)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Map as M
import Control.Monad (replicateM)

parse3 :: M.Map Char Bool -> [Token] -> [Bool] -> Maybe (M.Map Char Bool, [Token], Bool)
parse3 m (Var c:ts) b = Just $ if isNothing (m M.!? c) then let tb = b !! M.size m in (M.insert c tb m, ts, tb) else (m, ts, fromJust $ M.lookup c m)
parse3 m (Not:ts) b = third not <$> parse3 m ts b
parse3 m (LParen:ts) b = flip (maybe Nothing) (parse1 m ts b) $ \(m1, tk1, nb) -> if null tk1 || head tk1 /= RParen then Nothing else Just (m1, tail tk1, nb)
parse3 _ _ _ = Nothing

parse2 :: M.Map Char Bool -> [Token] -> [Bool] -> Maybe (M.Map Char Bool, [Token], Bool)
parse2 m tk b = maybe Nothing f $ parse3 m tk b
    where
        f t@(mm, ttk, bb)
            | not (null ttk) && head ttk == Mul = maybe Nothing (f . third (bb &&)) $ parse3 mm (tail ttk) b
            | otherwise = Just t

parse1 :: M.Map Char Bool -> [Token] -> [Bool] -> Maybe (M.Map Char Bool, [Token], Bool)
parse1 m tk b = maybe Nothing f $ parse2 m tk b
    where
        f t@(mm, ttk, bb)
            | not (null ttk) && head ttk == Add = maybe Nothing (f . third (bb ||)) $ parse2 mm (tail ttk) b
            | otherwise = Just t

parse0 :: [Token] -> [[Bool]] -> Maybe [(M.Map Char Bool, Bool)]
parse0 ts bs
    | all isJust l = Just $ map (first fst3 . second thd3 . dupe . fromJust) l 
    | otherwise = Nothing
    where
        l = map (parse1 M.empty ts) bs

{-# INLINE allPattern #-}
allPattern :: String -> Maybe [(M.Map Char Bool, Bool)]
allPattern = maybe Nothing (uncurry id . first parse0 . second (flip replicateM [True, False] . varnum) . dupe) . tokenize
