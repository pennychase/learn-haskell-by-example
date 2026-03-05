module Test.SimpleCheck where

import System.Random
import System.Random.Stateful


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
propertyTest :: Show a => (a -> b) -> (b -> Bool) -> IO a -> Int -> IO ()
propertyTest fun predicate random n
    | n <= 0 = putStrLn "Tests successful"
    | otherwise = do
        testCase <- random
        if predicate $ fun testCase
            then propertyTest fun predicate random $ n - 1
            else putStrLn $ "Test failed on: " <> show testCase

-- Random value generators

newtype RandomIO a = RandomIO {runRandomIO :: IO a}

one :: Random a => RandomIO a
one = RandomIO $ applyGlobalStdGen random

some ::Random a => RandomIO [a]
some = RandomIO $ do
    n <- applyGlobalStdGen $ uniformR (0, 100)
    replicateIO n $ runRandomIO one

replicateIO :: Int -> IO a -> IO [a]
replicateIO n act
    | n <= 0 = return []
    | otherwise = do
        x <- act
        xs <- replicateIO (n - 1) act
        return $ x : xs
