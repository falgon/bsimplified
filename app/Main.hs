module Main where

import BSimplified.Parse
import BSimplified.Bits (DigitListBits (..))
import BSimplified.QMM (pdnfForest, traceMinTermsStr)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Tuple.Extra (first, second, dupe)
import Data.Bool (bool)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import qualified Data.Map as M

prints :: (Show a, Show b) => (a -> b) -> a -> IO a
prints f x = x <$ print (f x)

traceCompute :: String -> IO ()
traceCompute = flip (.) allPattern $ maybe (return ()) $ \x -> let varname = map ((++) "m_" . show) $ take (length x) (iterate (+1) 0) in do
    putStrLn "Minterms (Truth patterns):"
    let l = filter ((==True) . snd . fst) $ zip x varname
        fr = fromJust $ flip pdnfForest [] $ map (DigitListBits . map (fromEnum . snd) . M.toList . fst . fst) l
    mapM_ ((>>) (putStr "\t") . putStrLn) $ uncurry (zipWith (\v y -> v ++ " = { " ++ intercalate ", " y ++ " }")) $ first (map snd) $ second (map (map (uncurry (:) . second ((++) " = " . show)) . M.toList . fst . fst)) $ dupe l
    traceMinTermsStr fr (map snd l) >>= maybe (return ()) ((>>) (putStr "Simplified terms: ") . (putStrLn . intercalate ", " . map (flip (++) ")" . ((++) "(" . unwords))))

main :: IO ()
main = uncurry (bool exitFailure . (traceCompute . head)) . second ((==1) . length) . dupe =<< getArgs
