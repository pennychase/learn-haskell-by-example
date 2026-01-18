module Csv.Operations
    ( foldCsv,
      filterCsv,
      countNonEmpty,
      countOccurrences,
      searchText,
      compareValue,
      countComparisons
    )
where

import qualified Data.List as L
import qualified Data.Text as T

import Csv.Types


foldCsv :: (DataField -> b -> b) -> b -> Csv -> [b]
foldCsv f z (Csv {csvColumns}) = map (foldr f z) csvColumns

filterCsv :: (DataField -> Bool) -> Csv -> Csv
filterCsv p csv@(Csv {csvColumns}) =
    let rows = L.transpose csvColumns
        filtered = L.filter (any p) rows
    in csv { csvColumns = L.transpose filtered }

countNonEmpty :: Csv -> [Int]
countNonEmpty = foldCsv f 0
    where
        f NullValue acc = acc
        f _ acc = acc + 1

countOccurrences :: DataField -> Csv -> [Int]
countOccurrences df =
    foldCsv (\x acc -> if x == df then acc + 1 else acc) 0

searchText :: T.Text -> Csv -> Csv
searchText t = filterCsv (\f -> dataFieldToText f `contains` t)
    where
        contains = flip T.isInfixOf

-- filter CSV by a numeric comparison
compareValue :: (Int -> Bool) -> Csv -> Csv
compareValue f = filterCsv f'
    where
        f' (IntValue n) = f n
        f' _ = False

-- count comparisons in each column
countComparisons :: (Int -> Bool) -> Csv -> [Int]
countComparisons f = foldCsv f' 0
    where
        f' (IntValue n) acc = if f n then acc + 1 else acc
        f' _ acc = acc

