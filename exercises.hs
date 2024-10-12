-- Exercises for Learn Haskell By Example
-- These are exercises that aren't part of the projects

-- Chapter 2
-- Implement lines, unlines, words, unwords

-- unlines and unwords behave identically, except for the different separator, so write a 
-- general splitting function
splitOn :: Char -> String -> [String]
splitOn ch str =
    let go :: String -> String -> [String]
        go acc [] = [acc]
        go acc (c : cs)
            | c == ch = acc : go [] cs
            | otherwise = go (acc <> [c]) cs
    in go [] str

myLines :: String -> [String]
myLines = splitOn '\n'

myWords :: String -> [String]
myWords = splitOn ' '

-- unlines adds '\n' as the end of the string, while unwords doesn't add a space. So need to write
-- separate functions
myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines (x : xs) = x <> "\n" <> myUnlines xs

myUnwords :: [String] -> String
myUnWords [] = ""
myUnwords (x : xs)
    | null xs = x
    | otherwise = x <> " " <> myUnwords xs

