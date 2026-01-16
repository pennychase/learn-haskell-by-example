import Data.Maybe
import Data.List

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


myMapM :: (a -> IO b) -> [a] -> IO [b]
myMapM _f [] = do
    return []

myMapM f (x : xs) = do
    result <- f x
    results <- myMapM f xs
    return $ result : results

myMapM_ :: (a -> IO b) -> [a] -> IO ()
myMapM_ f xs = do
    _ <- myMapM f xs
    return ()

-- Chapter 5

member :: Eq a => a -> [(a, b)] -> Bool
member x map =
    case lookup x map of
        Nothing -> False
        Just _ -> True

alter :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
alter f key [] = maybe [] (\value  -> [(key, value)]) (f Nothing)
alter f key ((key', value') : xs)
    | key == key' = maybe xs (\value -> (key, value) : xs ) (f (Just value'))
    | otherwise = (key', value') : alter f key xs

-- Chapter 8

-- implement Foldable convenience functions using foldr

myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1) 0

myElem :: (Eq a) => a -> [a] -> Bool
myElem y = foldr (\x acc -> (x == y) || acc) False

mySum :: (Num a) => [a] -> a
mySum = foldr (+) 0

myProduct :: (Num a) => [a] -> a
myProduct = foldr (*) 1

myMinimum :: (Ord a, Bounded a) => [a] -> a
myMinimum = foldr (\n acc -> min n acc) maxBound

myMaximum :: (Ord a, Bounded a) => [a] -> a
myMaximum = foldr (\n acc -> max n acc) minBound

-- Foldable instance for Tree

-- Minimum definition for a foldable instance is foldMap | foldr
-- We'll implement both for practice. They walk the Tree in
-- in order.

data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving (Show)

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f (Leaf a) = f a
    foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f acc (Leaf a)  = f a acc
    foldr f acc (Node l a r) = foldr f (f a (foldr f acc r)) l

inOrder :: Tree a -> [a]
inOrder = foldr (:) []






