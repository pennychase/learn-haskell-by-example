-- Exercises for Learn Haskell By Example
-- These are exercises that aren't part of the projects

--
-- Chapter 4
--

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

-- Implement zipWith and use it to define zip

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith' (,)

-- Implement mapM and mapM_

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f as = 
    let go acc ys = do
            case ys of
                [] -> return acc
                (x : xs) -> do
                    b <- f x
                    go (b : acc) xs
    in go [] as

mapM_' :: (a -> IO b) -> [a] -> IO ()
mapM_' _ [] = return ()
mapM_' f (x : xs) = do
    f x
    mapM_' f xs

--
-- Chapter 5
--

-- Implement member with lookup
member :: Eq a => a -> [(a,b)] -> Bool
member x xs =
    case lookup x xs of
        Nothing -> False
        Just _ -> True

-- Implement alter with maybe

alter :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)]-> [(k, v)]
alter f key [] = maybe [] (\value -> [(key, value)]) (f Nothing)
        
alter f key ((key', value') : xs)
    | key == key' = maybe xs (\value -> (key, value) : xs) (f (Just value'))
    | otherwise = (key', value') : alter f key xs


-- High order function to implment addEdges and buildDiGraph, i.e., write foldr

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f x [] = x
myFoldr f x (y : ys) = f y (myFoldr f x ys)


