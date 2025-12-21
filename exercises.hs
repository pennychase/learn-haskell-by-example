import Data.Maybe

-- Chapter 2

(!!!) :: [a] -> Int -> a
(!!!) xs n
    | n < 0 = undefined
    | n >= length xs = undefined
    | otherwise = go xs n
    where
        go (x : xs) n
            | n == 0 = x
            | otherwise = go xs (n - 1)

-- Chapter 3

indexOf :: Char -> [Char] -> Maybe Int
indexOf ch str = go ch str 0
    where
        go :: Char -> [Char] -> Int -> Maybe Int
        go ch [] n = Nothing
        go ch (x : xs) n
            | ch == x = Just n
            | otherwise = go ch xs (n + 1)