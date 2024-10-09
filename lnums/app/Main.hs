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

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe 
    (printHelpText "Missing filename")
    (\filePath -> putStrLn filePath)
    mFilePath

