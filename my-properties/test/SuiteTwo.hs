{-# LANGUAGE TemplateHaskell #-}

module SuiteTwo where

import Test.QuickCheck

prop_addPos :: Int -> Int -> Property
prop_addPos x y =
    withNumTests 500 $
        x > 0 && y > 0 ==> x + y > 0

prop_multZero :: Int -> Property
prop_multZero x =
    noShrinking $
        cover 95 ( x/= 0) "non-zero" $ x * 0 == 0

return []

runTests :: IO Bool
runTests = $quickCheckAll

