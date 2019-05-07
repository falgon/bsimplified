module BSimplified.Lexer (
    TokenKind (..),
    tokenized,
    varCount
) where

import Data.List (unfoldr)
import Data.Char (isAlpha)

data TokenKind = Lvar { varname :: Char } | Land | Lor | Lnot | Llbrace | Lrbrace | Leof | Linvalid deriving (Show, Eq)

tokenize :: String -> (TokenKind, String)
tokenize = tokenize' . filter (/=' ')
    where
        tokenize' [] = (Leof, [])
        tokenize' ('&':xs) = (Land, xs)
        tokenize' ('|':xs) = (Lor, xs)
        tokenize' ('\'':xs) = (Lnot, xs)
        tokenize' ('(':xs) = (Llbrace, xs)
        tokenize' (')':xs) = (Lrbrace, xs)
        tokenize' (x:xs) | isAlpha x = (Lvar x, xs) | otherwise = (Linvalid, xs)

tokenized :: String -> [TokenKind]
tokenized = (++ [Leof]) . unfoldr (\xs -> if null xs then Nothing else Just $ tokenize xs)

isValid :: [TokenKind] -> Bool
isValid = all (/=Linvalid)

varCount :: [TokenKind] -> Int
varCount = length . filter (\x -> case x of Lvar _ -> True; _ -> False)
