module Main (main) where

import qualified Csv
import qualified Data.Either as E
import qualified Data.Maybe as M
import Data.Sliceable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Utils.Arguments as Args

parseInFile :: T.Text -> IO (Either String Csv.Csv)
parseInFile key = do
  mInFile <- Args.getText key
  mFieldSep <- Args.getChar "field-separator"
  hasHeader <- Args.getBool "with-header"

  let 
    separators =
      Csv.defaultSeparators 
        {
          Csv.sepFieldSeparator  = M.fromMaybe (Csv.sepFieldSeparator Csv.defaultSeparators) mFieldSep 
        }

    headerOpt = 
      if hasHeader
        then Csv.WithHeader
        else Csv.WithoutHeader

    parseOpts =
      Csv.CsvParseOptions 
        {
          Csv.cpoSeparators = separators,
          Csv.cpoHeaderOption = headerOpt
        }

  case mInFile of
    Just inFile -> do
      contents <- TIO.readFile $ T.unpack inFile
      return $ Csv.parseCsv parseOpts contents
    _ -> return $ Left "argument not set"



main :: IO ()
main = do
  mCsv <- parseInFile "in"
  case mCsv of
    Left _ -> putStrLn "no input file given (do so with --in=...)"
    Right csv -> do
      mAppend <- eitherToMaybe <$> parseInFile "append"   -- get the arguments (Maybe values)
      mSliceInterval <- Args.getInterval "slice"
      mSearch <- Args.getText "search"

      let                                                   -- map argument values to functions
        mAppendOp = fmap (flip (<>)) mAppend
        mSliceOp = fmap (uncurry slice) mSliceInterval
        mSearchOp = fmap Csv.searchText mSearch
        transformOp =
          foldl (\t mOp -> (M.fromMaybe id mOp) . t) id [mAppendOp, mSliceOp, mSearchOp]
        transformedCsv = transformOp csv

      mOut <- Args.getText "out"
      case mOut of
        Just "-" -> Csv.printCsv transformedCsv
        Just fp -> Csv.writeCsv (T.unpack fp) transformedCsv
        _ -> do
          countNonEmpty <- Args.getBool "count-non-empty"
          let mSummary = if countNonEmpty
              then Just . fmap (T.pack . show) $ Csv.countNonEmpty transformedCsv
              else Nothing
          noPrettyOut <- Args.getBool "no-pretty"
          unless noPrettyOut $
            TIO.putStrLn $
              Csv.prettyText $
                maybe id (flip Csv.unsafeWithSummaries) mSummary (Csv.fromCsv transformedCsv)


eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

when :: Bool -> IO () -> IO ()
when True act = act
when False _ = return ()

unless :: Bool -> IO () -> IO ()
unless b = when (not b)

