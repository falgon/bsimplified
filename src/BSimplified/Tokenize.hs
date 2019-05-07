module BSimplified.Tokenize (
    Token (..),
    tokenize,
    varMaybe,
    varnum
) where

import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.Char (isAlpha)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Tuple.Extra (second, dupe)
import qualified Data.Set as S

data Token = Var Char | Not | LParen | RParen | Add | Mul deriving (Eq, Show)

{-# INLINE tokenTable #-}
tokenTable :: [(Char, Token)]
tokenTable = [('(', LParen), (')', RParen), ('+', Add), ('*', Mul), ('~', Not)]

{-# INLINE varMaybe #-}
varMaybe :: Token -> Maybe Char
varMaybe (Var c) = Just c
varMaybe _ = Nothing

{-# INLINE tokenize #-}
tokenize :: String -> Maybe [Token]
tokenize = uncurry (bool Nothing . Just . map fromJust) . second (all isJust) . dupe . map (\x -> lookup x tokenTable <|> (if isAlpha x then Just (Var x) else Nothing))

{-# INLINE varnum #-}
varnum :: [Token] -> Int
varnum = S.size . foldr (\x acc -> maybe acc (`S.insert` acc) $ varMaybe x) S.empty
