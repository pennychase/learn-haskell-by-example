{-# LANGUAGE TemplateHaskell #-}

module Main (main) where


import Composition.Performance
import Composition.Pitch
import qualified Data.List as L
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck
import Util.Types

{-
Naive semitone property (adding or subtracting 12 results in double or half the frequency)
fails because of rounding)

prop_semitoneFreq :: Semitone -> Property
prop_semitoneFreq st =
  toFrequency st === toFrequency (st + 12) / 2 
    .&. toFrequency st === toFrequency (st - 12) * 2
-}

prop_semitoneFreq :: Semitone -> Property
prop_semitoneFreq st =
  abs (toFrequency st - (toFrequency (st + 12) / 2)) <= 0.000001
    .&. abs (toFrequency st - (toFrequency (st - 12) * 2)) <= 0.000001

return []

main :: IO ()
main = do
  res <- $quickCheckAll
  if res
    then exitSuccess
    else exitFailure


