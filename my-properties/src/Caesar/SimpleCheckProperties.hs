module Caesar.SimpleCheckProperties where

import Caesar.Caesar
import qualified Data.List as L
import Test.SimpleCheck

-- Note that having done the exercise to make make rot13 "work" with digits by creating
-- rot135, which does rot13 on letters (and non-alphanumeric asci) and rot on digits,
-- propRot13Symmetrical only works with letter strings. rot135 is symmetric for ascii Chars.
propRot13Symmetrical :: IO ()
propRot13Symmetrical =
    propertyTest
        (\s -> s == rot13 (rot13 s))
        letterString''
        100

propRot135Symmetrical :: IO ()
propRot135Symmetrical =
    propertyTest
        (\s -> s == rot135 (rot135 s))
        asciiString''
        100

propAlphabetRotClosed :: IO ()
propAlphabetRotClosed = propertyTest prop gen 100
    where
        prop (alphabet, n, c) =
            let c' = alphabetRot alphabet n c
            in c' `elem` alphabet

        gen = RandomIO $ do
            alphabet <- runRandomIO $ asciiString' `suchThat` (not . null)
            n <- runRandomIO $ elements [-100 .. 100]
            c <- runRandomIO $ elements alphabet
            return (L.nub alphabet, n, c)

propIsMiscIsDefault :: IO ()
propIsMiscIsDefault =
    propertyTest (\c -> isMisc c == not (isLower c || isUpper c || isDigit c))
    one
    10000
