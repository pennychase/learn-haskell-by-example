module Main(main) where

import Control.Monad
import Data.Maybe
import MyLib
import System.Environment

data LineNumberOption 
  = ReverseNumbering
  | SkipEmptyLines
  | SkipEmptyIncrement
  | LeftAlign
  | HelpText
  deriving (Eq, Show)

lnOptionFromString :: String -> Maybe LineNumberOption 
lnOptionFromString "--reverse" = Just ReverseNumbering
lnOptionFromString "--skip-empty" = Just SkipEmptyLines
lnOptionFromString "--skip-empty-incr" = Just SkipEmptyIncrement
lnOptionFromString "--left-align" = Just LeftAlign
lnOptionFromString "--help" = Just HelpText
lnOptionFromString _ = Nothing

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <options> <filename>")
  putStrLn "\n"
  putStrLn " Options:"
  putStrLn "    --reverse           - Reverse the numbering"
  putStrLn "    --skip-empty        - Skip numbering empty lines"
  putStrLn "    --skip-empty-incr   - Skip numbering empty lines and increment"
  putStrLn "    --left-align        - Use left-aligned line numbers"
  putStrLn "    --help              - Print this message"

parseArguments :: [String] -> (Maybe FilePath, [LineNumberOption])
parseArguments args = case reverse args of
  [] -> (Nothing, [])
  ["--help"] -> (Nothing, [HelpText])
  (filename : options) -> (Just filename, mapMaybe lnOptionFromString options)


readLines :: FilePath -> IO [String]
readLines filePath = do
  contents <- readFile filePath
  return (lines contents)

main :: IO ()
main = do
  cliArgs <- getArgs
  let (mFilePath, options) = parseArguments cliArgs
  when (HelpText `elem` options) (printHelpText "")

  let numberFunction
        | SkipEmptyLines `elem` options = numberNonEmptyLines
        | SkipEmptyIncrement `elem` options = numberAndIncrementNonEmptyLines 
        | otherwise = numberAllLines
      padMode =
        if LeftAlign `elem` options
          then PadRight
          else PadLeft
      go filePath = do
        fileLines <- readLines filePath
        let numbered = numberFunction fileLines
            prettyNumbered = prettyNumberedLines padMode numbered
            revNumbered = numberFunction (reverse fileLines)
            revPretty = reverse (prettyNumberedLines padMode revNumbered)
        mapM_ putStrLn (if ReverseNumbering `elem` options then revPretty else prettyNumbered)
  maybe
    (printHelpText "Missing filename")
    go
    mFilePath