module Csv.Types where

import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T

import Data.Sliceable(Sliceable(..))

data DataField 
  = IntValue Int 
  | TextValue T.Text 
  | NullValue
  deriving (Eq, Show)

type Column = [DataField]

data Csv = Csv
  { csvHeader :: Maybe [T.Text],
    csvColumns :: [Column]
  }
  deriving (Show)

instance Semigroup Csv where
  (<>) = appendCsv

instance Monoid Csv where
  mempty = Csv {csvHeader = Nothing, csvColumns = []}

instance Sliceable Csv where
  slicePartition idx1 idx2 Csv {..} =
    let (headerHd, headerSpl, headerTl) = slicePartition idx1 idx2 csvHeader
        (columnHd, columnSpl, columnTl) = slicePartition idx1 idx2 csvColumns
    in ( Csv { csvHeader = headerHd, csvColumns = columnHd},
         Csv { csvHeader = headerSpl, csvColumns = columnSpl},
         Csv { csvHeader = headerTl, csvColumns = columnTl}
        )

textToDataField :: T.Text -> DataField
textToDataField "" = NullValue
textToDataField raw =
  let mIntVale = readMaybe (T.umpack raw)
  in maybe (TextValue raw) IntValue mIntValue

dataFieldToText :: DataField -> T.Text
dataFieldToText NullValue = ""
dataFieldToText (IntValue i) = T.pack $ show i
dataFieldToText (TextValue t) = t 

mkCsv :: Maybe [T.Text] -> [Column] -> Either String Csv
mkCsv mHeader columns
  | not headerSizeCorrect =
      Left "Size of header row does not fit number of columns"
  | not columnSizesCorrect =
      Left "The columns do not have equal sizes"
  | otherwise = Right Csv {csvHeader = mHeader, csvColumns = columns}
  where
    headerSizeCorrect =
      M.maybe True (\h -> L.length h == L.length columns) mHeader
    columnSizesCorrect =
        L.length (L.nubBy (\x y -> length x == length y) columns) <= 1

unsafeMkCsv :: Maybe [T.Text] -> [Column] -> Csv
unsafeMkCsv header columns = E.either error id (mkCsv header columns)

numberOfRows :: Csv -> Int
numberOfRows Csv {..} =
  case csvColumns of
    [] -> 0
    (x : _) -> length x

numberOfColumns :: Csv -> Int
numberOfColumns Csv {..} = length csvColumns

appendCsv :: Csv -> Csv -> Csv
appendCsv a b =
  Csv
    { csvHeader =
        if M.isNothing (csvHeader a) && M.isNothing (csvHeader b)
          then Nothing
          else Just $ header' a ++ header' b,
      csvColumns = appendColumns (csvColumns a) (csvColumns b)
    }
  where
    header' csv = M.fromMaybe (L.replicate (numberOfColumns csv) T.empty) (csvHeader csv)

    appendColumns colsA colsB =
      map (\cols -> cols ++ fillA) colsA ++ map (\cols -> cols ++ fillB) colsB
      where
        fillA = replicate (numberOfRows b - numberOfRows a) NullValue
        fillB = replicate (numberOfRows a - numberOfRows b) NullValue


  



