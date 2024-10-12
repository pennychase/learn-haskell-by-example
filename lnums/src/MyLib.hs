module MyLib where

import Data.Char

type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]

-- predicates to test if a string contains at least one printable character, or not
isEmpty :: String -> Bool
isEmpty str =
    null str
        || all (\s -> not (isPrint s) || isSeparator s) str

notIsEmpty :: String -> Bool
notIsEmpty = not . isEmpty

-- generalized higher-order function for nunbering lines
numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber text =
    let go :: Int -> [String] -> NumberedLines
        go _ [] = []
        go counter (x : xs) = 
            let mNumbering = if shouldNumber x then (Just counter) else Nothing
                newCounter = if shouldIncr x then counter + 1 else counter
            in (mNumbering, x) : go newCounter xs
    in go 1 text

-- number all lines
numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

-- number non-empty lines but increment counter for all lines
numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) notIsEmpty

-- number non-empty lines without incrementing the counter for empty lines
numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines notIsEmpty notIsEmpty