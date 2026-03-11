module Main where

import qualified SuiteOne as S1
import qualified SuiteTwo as S2
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    s1success <- S1.runTests
    s2success <- S2.runTests
    if s1success && s2success
        then exitSuccess
        else exitFailure