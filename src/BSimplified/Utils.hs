module BSimplified.Utils (
    insertAt,
    replaceAt,
    replaceAtIgnored,
    diffIndices,
    findDelete,
    third
) where

import Data.List (elemIndices)

insertAt :: a -> [a] -> Int -> [a]
insertAt v xs 0 = v : xs
insertAt v (x:xs) n = x : insertAt v xs (n - 1)
insertAt v [] _ = [v]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt i v xs 
    | i < length xs = take i xs ++ [v] ++ drop (succ i) xs 
    | otherwise = []

replaceAtIgnored :: Eq a => Int -> a -> a -> [a] -> [a]
replaceAtIgnored = go 0
    where
        go n i v ig (x:xs)
            | n < i && x /= ig = x : go (succ n) i v ig xs
            | n < i && x == ig = x : go n i v ig xs
            | n == i = v : xs
            | otherwise = []
        go _ _ _ _ [] = []

{-# INLINE diffIndices #-}
diffIndices :: Eq a => [a] -> [a] -> [Int]
diffIndices = (.) (elemIndices False) . zipWith (==)

{-# INLINE findDelete #-}
findDelete :: (a -> Bool) -> [a] -> Maybe (a, [a])
findDelete = fd []
    where
        fd _ _ [] = Nothing
        fd ls f (x:xs) | f x = Just (x, ls ++ xs) | otherwise = fd (ls ++ [x]) f xs

{-# INLINE third #-}
third :: (c -> d) -> (a, b, c) -> (a, b, d)
third f (x, y, z) = (x, y, f z)
