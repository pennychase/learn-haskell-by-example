module Test.SimpleCheck where

import System.Random
import System.Random.Stateful
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M


sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : y : ys) = x <= y && sorted (y : ys)

sorts :: Ord a => ([a] -> [a]) -> [a] -> Bool
sorts f input = sorted $ f input

-- Produce random lists of random sizes

-- Produce a random list of size n
randomListN :: (Random a) => StdGen -> Int -> ([a], StdGen)
randomListN gen n
    | n <= 0 = ([], gen)
    | otherwise =
        let (v, gen') = random gen
            (xs, gen'') = randomListN gen' (n - 1)
        in (v : xs, gen'')

-- Randomly select the size of the list, and produce random list of that size using randomListN
randomList :: (Random a) => StdGen -> Int -> ([a], StdGen)
randomList gen maxVal = randomListN gen' n
    where
        (n, gen') = uniformR (0, maxVal) gen

-- Produce a random list of size bewteen 0 and 100
randomList' :: (Random a) => StdGen -> ([a], StdGen)
randomList' = flip randomList 100

-- IO Action that generates random lists using a global StdGen 
-- Second version is safe for concurrency, since uses applyAtomicGen
randomListIO :: (Random a) => IO [a]
randomListIO = do
    g <- getStdGen
    let (xs, g') = randomList' g
    setStdGen g'
    return xs

randomListIO' :: (Random a) => IO [a]
randomListIO' = applyAtomicGen randomList' globalStdGen 

-- Convenience function to generate random values
applyGlobalStdGen :: (StdGen -> (a, StdGen)) -> IO a
applyGlobalStdGen f = applyAtomicGen f globalStdGen

-- Property Tests

-- Specific test for sorting
propertyTestSorts :: ([Int] -> [Int]) -> Int -> IO ()
propertyTestSorts f n 
    | n <= 0 = putStrLn "Tests successful"
    | otherwise = do
        xs <- applyGlobalStdGen randomList'
        if f `sorts` xs
            then propertyTestSorts f $ n - 1
            else putStrLn $ "Test failed on: " <> show xs

-- Generic property test IO action
propertyTest :: Show a => (a -> Bool) -> RandomIO a -> Int -> IO ()
propertyTest predicate random n
    | n <= 0 = putStrLn "Tests successful"
    | otherwise = do
        testCase <- runRandomIO random
        if predicate testCase
            then propertyTest predicate random $ n - 1
            else putStrLn $ "Test failed on: " <> show testCase

-- Random value generators

newtype RandomIO a = RandomIO {runRandomIO :: IO a}

instance Functor RandomIO where
    fmap f rand = RandomIO $ do 
        val <- runRandomIO rand
        return $ f val

one :: Random a => RandomIO a
one = RandomIO $ applyGlobalStdGen random

some ::Random a => RandomIO [a]
some = RandomIO $ do
    n <- applyGlobalStdGen $ uniformR (0, 100)
    replicateIO n $ runRandomIO one

manyOf :: RandomIO a -> RandomIO [a]
manyOf rio = RandomIO $ do
    n <- applyGlobalStdGen $ uniformR (0, 100)
    replicateIO n (runRandomIO rio)

replicateIO :: Int -> IO a -> IO [a]
replicateIO n act
    | n <= 0 = return []
    | otherwise = do
        x <- act
        xs <- replicateIO (n - 1) act
        return $ x : xs

suchThat :: RandomIO a -> (a -> Bool) -> RandomIO a
suchThat rand pred = RandomIO $ do
    val <- runRandomIO rand
    if pred val
        then return val
        else runRandomIO $ suchThat rand pred

-- Generators
nonNegative :: (Num a, Ord a, Random a) => RandomIO a
nonNegative = one `suchThat` (> 0)

nonEmpty :: Random a => RandomIO [a]
nonEmpty = some `suchThat` (not . null)

-- ASCII chars and strings

asciiChar :: RandomIO Char
asciiChar = one `suchThat` (\c -> isAscii c && not (isControl c))

letterChar :: RandomIO Char
letterChar = asciiChar `suchThat` isLetter

asciiString :: RandomIO String
asciiString = manyOf asciiChar

letterString :: RandomIO String
letterString = manyOf letterChar

-- More efficient character and string generators

-- Use a list
elements :: [a] -> RandomIO a
elements [] = error "elements cannot work with an empty list"
elements xs = RandomIO $ do
  i <- applyGlobalStdGen $ uniformR (0, length xs - 1)
  return $ xs !! i

asciiChar' :: RandomIO Char
asciiChar' = elements ['\x20' .. '\x7e']

letterChar' :: RandomIO Char
letterChar' = elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

asciiString' :: RandomIO String
asciiString' = manyOf asciiChar'

letterString' :: RandomIO String
letterString' = manyOf letterChar'

-- Use a map

elements' :: Map Int a -> RandomIO a
elements' m = RandomIO $ do
  i <- applyGlobalStdGen $ uniformR (0, M.size(m) - 1)
  return $ m ! i

asciiCharMap :: Map Int Char
asciiCharMap = M.fromList $ zip [0..] ['\x20' .. '\x7e']

asciiChar'' :: RandomIO Char
asciiChar'' = elements' asciiCharMap

asciiString'' :: RandomIO String
asciiString'' = manyOf asciiChar''

letterMap :: Map Int Char
letterMap = M.fromList $ zip [0..] "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

letterChar'' :: RandomIO Char
letterChar'' = elements' letterMap

letterString'' :: RandomIO String
letterString'' = manyOf letterChar''