module Lib where

import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import qualified Data.MultiSet as MS

type Alphabet = [Char]

--
-- Definitions for Latin alphabets and digits
--

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

--
-- Helper functions to check alphabet membership
--

isLower :: Char -> Bool
isLower c = c `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper c = c `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit c = c `elem` digits

isMisc :: Char -> Bool
isMisc c = c `notElem` lowerAlphabet ++ upperAlphabet ++ digits

--
-- List functions
--

listLength :: [a] -> Int
listLength [] = 0
listLength (x : xs) = 1 + listLength xs

-- This function will fail if the character isn't in the list
-- We'll assume that's the case for now
indexOf :: Char -> Alphabet -> Int
indexOf c [] = undefined
indexOf c (x : xs) = if x == c then 0 else 1 + indexOf c xs

(!!) :: [a] -> Int -> a
[] !! _ = undefined
(x : xs) !! n = if n == 0 then x else xs Lib.!! (n - 1)

--
-- Rotation functions
--

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n c =
    alphabet Lib.!! ((indexOf c alphabet + n) `mod` listLength alphabet)

upperRot :: Int -> Char -> Char
upperRot n c = alphabetRot upperAlphabet n c

lowerRot :: Int -> Char -> Char
lowerRot n c = alphabetRot lowerAlphabet n c

rotChar :: Int -> Char -> Char
rotChar n c
    | isLower c = lowerRot n c
    | isUpper c = upperRot n c
    | otherwise = c

--
-- Caesar's Cipher
--

caesar :: Int -> String -> String
caesar n x = map (rotChar n) x

--
-- Exercise: ROT135 Cipher
--

digitRot :: Int -> Char -> Char
digitRot n c = alphabetRot digits n c

rotAlphaDigit :: Char -> Char
rotAlphaDigit c
    | isLower c = lowerRot 13 c
    | isUpper c = upperRot 13 c
    | isDigit c = digitRot 5 c
    | otherwise = c

rot135 :: String -> String
rot135 = map rotAlphaDigit

--
-- Exercise: Frequency analysis of strings
--
count :: String -> [(Char, Int)]
count str =
    let
        counts = MS.fromList str
    in sortBy (comparing (Data.Ord.Down . snd)) $ MS.toAscOccurList counts



