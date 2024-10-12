module Main (main) where

import System.Environment

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")

parseArguments :: [String] -> Maybe FilePath
parseArguments [filepath] = Just filepath
parseArguments _ = Nothing

readLines :: FilePath -> IO [String]
readLines filepath = do
  contents <- readFile filepath
  return (lines contents)

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe 
    (printHelpText "Missing filename")
    (\filePath -> putStrLn filePath)
    mFilePath
  
  

