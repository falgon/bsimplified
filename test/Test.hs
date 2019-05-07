module Test (
    Test (..),
    runTest
) where

import Test.HUnit (Test (..), runTestText, putTextToHandle)
import System.IO (stderr)
import Control.Monad (void)

runTest :: Test -> IO ()
runTest = void . runTestText (putTextToHandle stderr False)
