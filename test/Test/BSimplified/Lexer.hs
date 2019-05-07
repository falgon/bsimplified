module Test.BSimplified.Lexer (
    tokenizedTest1,
    tokenizedTest2,
    tokenizedTest3,
    isValidTest1,
    isValidTest2,
    isValidTest3,
    varCountTest1,
    varCountTest2,
    varCountTest3
) where

import Test.HUnit (Test, (@?=), (~:))
import qualified BSimplified.Lexer as BL

testData :: [String]
testData = ["(A&B&C')|D", "A   | B'&  C''", "A?C"]

-- tokenized
tokenizedTest :: Int -> String -> [BL.TokenKind] -> Test
tokenizedTest n st ans = "tokenized " ++ show n ~: BL.tokenized st @?= ans

tokenizedTest1 :: Test
tokenizedTest1 = tokenizedTest 1 (head testData) [BL.Llbrace, BL.Lvar 'A', BL.Land, BL.Lvar 'B', BL.Land, BL.Lvar 'C', BL.Lnot, BL.Lrbrace, BL.Lor, BL.Lvar 'D', BL.Leof]

tokenizedTest2 :: Test
tokenizedTest2 = tokenizedTest 2 (testData !! 1) [BL.Lvar 'A', BL.Lor, BL.Lvar 'B', BL.Lnot, BL.Land, BL.Lvar 'C', BL.Lnot, BL.Lnot, BL.Leof]

tokenizedTest3 :: Test
tokenizedTest3 = tokenizedTest 3 (last testData) [BL.Lvar 'A', BL.Linvalid, BL.Lvar 'C', BL.Leof]

-- isValid
isValidTest :: Int -> String -> Bool -> Test
isValidTest n st b = "isValidTest " ++ show n ~: BL.isValid (BL.tokenized st) @?= b

isValidTest1 :: Test
isValidTest1 = isValidTest 1 (head testData) True

isValidTest2 :: Test
isValidTest2 = isValidTest 2 (testData !! 1) True

isValidTest3 :: Test
isValidTest3 = isValidTest 3 (last testData) False

-- varCount
varCountTest :: Int -> String -> Int -> Test
varCountTest n st ans = "varCountTest " ++ show n ~: BL.varCount (BL.tokenized st) @?= ans

varCountTest1 :: Test
varCountTest1 = varCountTest 1 (head testData) 4

varCountTest2 :: Test
varCountTest2 = varCountTest 2 (testData !! 1) 3

varCountTest3 :: Test
varCountTest3 = varCountTest 3 (last testData) 2
