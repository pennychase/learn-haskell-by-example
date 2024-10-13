module MyLib 
    ( NumberedLine,
      NumberedLines,
      isEmpty,
      isNotEmpty,
      numberLines,
      numberAllLines,
      numberNonEmptyLines,
      numberAndIncrementNonEmptyLines,
      prettyNumberedLines,
      PadMode (..),
      pad,
      padLeft,
      padRight,
      padCenter
    )
where

import Data.Char

type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]

--
-- Line Numbering 
--

-- predicates to test if a string contains at least one printable character, or not
isEmpty :: String -> Bool
isEmpty str =
    null str
        || all (\s -> not (isPrint s) || isSeparator s) str

isNotEmpty :: String -> Bool
isNotEmpty = not . isEmpty

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
numberNonEmptyLines = numberLines (const True) isNotEmpty

-- number non-empty lines without incrementing the counter for empty lines
numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines isNotEmpty isNotEmpty


--
-- Padding Lines
--

data PadMode = PadLeft | PadRight | PadCenter

pad :: PadMode -> Int -> String -> String
pad mode n str = 
    let diff = n - length str
        diff' = diff `div` 2
        padding = replicate diff ' '
        paddingLeft = if even diff then replicate diff' ' ' else replicate (diff' + 1) ' '
        paddingRight = replicate diff' ' '
    in case mode of
        PadLeft -> padding ++ str
        PadRight -> str ++ padding
        PadCenter -> paddingLeft ++ str ++ paddingRight

padLeft :: Int -> String -> String
padLeft = pad PadLeft

padRight :: Int -> String -> String
padRight = pad PadRight

padCenter :: Int -> String -> String
padCenter = pad PadCenter

--
-- Printing numbered lines
--

prettyNumberedLines :: PadMode -> NumberedLines -> [String]
prettyNumberedLines mode lineNums = 
    let (numbers, text) = unzip lineNums
        numberStrings = map (maybe "" show) numbers
        maxLength = maximum (map length numberStrings)
        paddedNumbers = map (pad mode maxLength) numberStrings
    in zipWith (\n l -> n ++ " " ++ l) paddedNumbers text

