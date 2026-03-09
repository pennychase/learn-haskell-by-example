module Caesar.QuickCheckProperties where

import Caesar.Caesar
import qualified Data.List as L
import Test.QuickCheck

-- Initial version
prop_alphabetRotClosed' :: Alphabet -> Int -> Char -> Bool
prop_alphabetRotClosed' alphabet n c =
    let c' = alphabetRot alphabet n c
    in c' `elem` alphabet

-- Property test for closedness using QuickCheck functionality

prop_alphabetRotClosed :: Property
prop_alphabetRotClosed =
    forAll gen prop
    where
        prop :: (Alphabet, Int, Char) -> Bool
        prop (alphabet, n, c) =
            let c' = alphabetRot alphabet n c
            in c' `elem` alphabet

        gen :: Gen (Alphabet, Int, Char)
        gen = do
            size <- getSize
            alphabet <- arbitrary `suchThat` (not . null)
            n <- choose (-size, size)
            c <- elements alphabet
            return (L.nub alphabet, n, c)

