module Main where

import Test (Test (..), runTest)
import qualified Test.BSimplified.Lexer as TBL

main :: IO ()
main = runTest $ TestList [
    TBL.tokenizedTest1, 
    TBL.tokenizedTest2,
    TBL.tokenizedTest3,
    TBL.isValidTest1,
    TBL.isValidTest2,
    TBL.isValidTest3,
    TBL.varCountTest1,
    TBL.varCountTest2,
    TBL.varCountTest3
    ]
