{-# LANGUAGE TemplateHaskell #-}

module SuiteOne where

import Test.QuickCheck

prop_true :: Int -> Bool
prop_true = const True

prop_false :: Int -> Bool
prop_false = const False

return []

runTests :: IO Bool
runTests = $quickCheckAll