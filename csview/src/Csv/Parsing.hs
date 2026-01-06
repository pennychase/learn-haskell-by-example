module Csv.Parsing where

data Separators = Separators
    { sepLineSeparator :: Char,
      sepFieldSeparator :: Char
    }
    deriving (Eq, Show)

data HeaderOption = WithHeader | WithoutHeader
    deriving (Eq, Show)

data CsvParseOptions = CsvParseOptions
    { cpoSeparators :: Separators,
      cpoHeaderOption :: HeaderOption
    }

defaultSeparators :: Separators
defaultSeparators = 
    Separators
        { sepLineSeparator = '\n',
          sepFieldSeparator = ','
    }

defaultOptions :: CsvParseOptions
defaultOptions =
    CsvParseOptions 
        { cpoSeparators = defaultSeparators,
          cpoHeaderOption = WithoutHeader
        }