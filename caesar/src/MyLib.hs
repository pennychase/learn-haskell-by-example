module MyLib where

import Data.Function(on)
import Data.List(minimumBy)

-- Define alphabets

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

-- Helper functions
isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit char = char `elem` digits

isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

indexOf :: Char -> Alphabet -> Int
indexOf _ch [] = undefined
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

toLower :: Char -> Char
toLower ch
    | isUpper ch = lowerAlphabet !! (indexOf ch upperAlphabet)
    | otherwise = ch

-- Rotation ciphers

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch = alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet)

upperRot :: Int -> Char -> Char
upperRot n ch = alphabetRot upperAlphabet n ch

lowerRot :: Int -> Char -> Char
lowerRot n ch = alphabetRot lowerAlphabet n ch

digitRot :: Int -> Char -> Char
digitRot n ch = alphabetRot digits n ch

rotChar :: Int -> Char -> Char
rotChar n ch
    | isLower ch = lowerRot n ch
    | isUpper ch = upperRot n ch
    | isDigit ch = digitRot n ch
    | otherwise = ch

caesar :: Int -> String -> String
caesar n message = map (\ch -> rotChar n ch) message

rot13 :: String -> String
rot13 = caesar 13

rot135 :: String -> String
rot135 = map rotChar135

rotChar135 :: Char -> Char
rotChar135 ch
    | isLower ch || isUpper ch = rotChar 13 ch
    | isDigit ch = rotChar 5 ch
    | otherwise = ch

-- Frequency Analysis
-- Based on Graham Hutton's _Programming in Haskell_, chapter 5

-- Frequencies of letters in English
frequencies :: [Float]
frequencies = [ 8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
                0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
                6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Char -> String -> Int
count ch xs = foldr (\x acc -> if x == ch then acc + 1 else acc) 0 xs

freqs :: String -> [Float]
freqs xs = map (\x -> percent (count x xs') n) lowerAlphabet
    where
        n = length xs
        xs' = map toLower xs

-- Chi Square is used to compare a list of observed frequencies with expected frequencies
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o, e) <- zip os es]

-- Cracking the cipher
-- Create frequency table for the cipher string. Rotate the table 0..25 places, 
-- calculate chi square for each shift, and find the minimum chi square. The index is
-- likely the rotation factor in the cipher.

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = caesar (-factor) xs
    where
        factor = fst (minimumBy (compare `on` snd ) chitab)
        chitab = [(n, chisqr (rotate n table) frequencies) | n <- [0..25] ]
        table = freqs xs



