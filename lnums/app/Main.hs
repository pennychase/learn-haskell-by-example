module Main (main) where

import Data.Maybe
import System.Environment
import MyLib

data LineNumberOption
  = Help
  | ReverseNumbering
  | SkipEmptyLines
  | JustNonEmptyLines
  | LeftAlign
  deriving (Eq, Show)

lnOptionFromString :: String -> Maybe LineNumberOption
lnOptionFromString "--help" = Just Help
lnOptionFromString "--reverse" = Just ReverseNumbering
lnOptionFromString "--skip-empty" = Just SkipEmptyLines
lnOptionFromString "--only-non-empty" = Just JustNonEmptyLines
lnOptionFromString "--left-align" = Just LeftAlign
lnOptionFromString _ = Nothing

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <options> <filename>")
  putStrLn "\n"
  putStrLn "Options: "
  putStrLn "   --help           - Show this help text"
  putStrLn "   --reverse        - Reverse the numbering"
  putStrLn "   --skip-empty     - Skip numbering empty lines"
  putStrLn "   --only-non-empty - Number only non-empty lines"
  putStrLn "   --left-align     - Use left-aligned line numbers"

parseArguments :: [String] -> (Maybe FilePath, [LineNumberOption])
parseArguments args =
  case reverse args of
    [] -> (Nothing, [])
    ["--help"] -> (Nothing, [Help])
    filename : options -> (Just filename, mapMaybe lnOptionFromString options)

readLines :: FilePath -> IO [String]
readLines filepath = do
  contents <- readFile filepath
  return (lines contents)

main :: IO ()
main = do
  cliArgs <- getArgs

  let (mFilePath, options) = parseArguments cliArgs

      helpMsg = if Help `elem` options then "" else "Missing filename"

      numberFunction
        | SkipEmptyLines `elem` options = numberNonEmptyLines
        | JustNonEmptyLines `elem` options = numberAndIncrementNonEmptyLines
        | otherwise = numberAllLines

      padMode 
        | LeftAlign `elem` options = PadRight
        | otherwise = PadLeft

      go filePath = do
        fileLines <- readLines filePath
        let numbered = numberFunction fileLines
            prettyNumbered = prettyNumberedLines padMode numbered
            revNumbered = numberFunction (reverse fileLines)
            revPretty = reverse (prettyNumberedLines padMode revNumbered)
        mapM_ putStrLn (if ReverseNumbering `elem` options then revPretty else prettyNumbered)
      
  maybe
    (printHelpText helpMsg)
    go
    mFilePath



