module Csv.Types where

import qualified Data.Text as Text

type DataField
    = IntValue Int
    | TextValue T.Text
    | NullValue
    deriving (Eq, Show)

type Column = [DataField]

data Csv = Csv
  { csvHeader :: Maybe [T.Text],
    csvColumns :: [Columns]
  }
  deriving (Show)


