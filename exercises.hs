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


-- Chapter 4

myWords :: String -> [String]
myWords str = split str ' '

myLines :: String -> [String]
myLines str = split str '\n'

split :: String -> Char -> [String]
split str char = 
    let go :: String -> Char -> String -> [String] -> [String]
        go "" _ cur  acc = reverse (reverse cur : acc)
        go (x : xs) ch cur acc 
            | x == ch = go xs ch "" (reverse cur : acc)
            | otherwise = go xs ch (x : cur) acc
    in go str char "" []

myUnLines :: [String] -> String
myUnLines strs = join strs "\n"

myUnWords :: [String] -> String
myUnWords wrds = join wrds " "

join :: [String] -> String -> String
join [] _ = ""
join (x : xs) s 
    | null xs = x
    | otherwise = x ++ s ++ join xs s


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : (myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _f [] _ = []
myZipWith _f _ [] = []
myZipWith f (x : xs) (y : ys) = (f x y) : (myZipWith f xs ys)

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)


