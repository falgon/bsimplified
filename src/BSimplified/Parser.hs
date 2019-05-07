module BSimplified.Parser (
) where

import qualified BSimplified.Lexer as BL

import Data.Maybe (maybe)
import Data.List (unfoldr)

{-
parse4 :: Bool -> [BL.TokenKind] -> Maybe ([BL.TokenKind], Bool)
parse4 _ [] = Nothing
parse4 val (x:xs)
    | x == BL.Llbrace = maybe Nothing (\(xs, b) -> if null xs || head xs /= BL.Lrbrace then Nothing else Just (tail xs, b)) $ parse0 (xs, val)
    | x == BL.Lnot = Just (xs, not 
-}

{-
parse4 :: Bool -> [BL.TokenKind] -> Maybe ([BL.TokenKind], Bool)
parse4 _ [] = Nothing
parse4 val (t:ts)
    | t == BL.Llbrace = flip (maybe Nothing) (parse0 (ts, val)) $ \(rs, b) -> if null xs || head rs /= BL.Lrbrace then Nothing else Just (tail rs, b)
    | t == BL.Lnot = Just (

parse1 :: [BL.TokenKind] -> Maybe ([BL.TokenKind], Bool)
parse1 xs = Just (xs, True)

parse0 :: [BL.TokenKind] -> Maybe ([BL.TokenKind], Bool)
parse0 tks = flip (maybe Nothing) (parse1 tks) $ \(xs, b) ->
    let res = unfoldr (\xxs -> if null xxs || head xxs /= BL.Lor then Nothing else maybe Nothing (\(ys, bb) -> Just ((bb, ys), ys)) (parse1 (tail xxs))) xs in 
        Just foldr (||) b (map snd res)

parse :: String -> Maybe Bool
parse [] = Nothing
parse xs = let tked = BL.tokenized xs in if not $ BL.isValid tked then Nothing else fmap snd $ parse0 tked
-}

{-
parse :: String -> Maybe Bool
parse xs = let tked = BL.tokenized xs in if not $ BL.isValid tked then Nothing else
    fmap (\(xs, b) -> if null xs then b else if head xs == Lor then ) $ 
    flip fmap (parse1 tked) $ \(xs, b) -> if null xs then b else foldr (||) $ unfoldr (\x -> if x == BL.Lor then Just (parse1 (init xs), init xs) else Nothing) xs
    -}
