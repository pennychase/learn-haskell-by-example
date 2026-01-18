module Utils.Arguments 
    ( getArguments,
      getValueOf,
      getBool,
      getText,
      getChar,
      getInterval
    )
where

import Prelude hiding(getChar)

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Text.Read as T
import System.Environment(getArgs)

getArguments :: IO [T.Text]
getArguments = map T.pack <$> getArgs

getValueOf :: T.Text -> IO (Maybe T.Text)
getValueOf key = do
    L.foldl getVal Nothing <$> getArguments
    where
        argKey = "--" <> key <> "="
        getVal mVal arg =
            if M.isNothing mVal
                then T.stripPrefix argKey arg
                else mVal

getBool :: T.Text -> IO Bool
getBool key =
    L.elem argKey <$> getArguments
    where
        argKey = "--" <> key

getText :: T.Text -> IO (Maybe T.Text)
getText = getValueOf

getChar :: T.Text -> IO (Maybe Char)
getChar key = do
    sep <- getValueOf key
    return $ case sep of
        Nothing -> Nothing
        Just str -> case T.uncons str of
            Just (c, rest) -> if T.null rest then Just c else Nothing
            _ -> Nothing

getInterval :: T.Text -> IO (Maybe (Int, Int))
getInterval key = do
    mVal <- fmap T.strip <$> getValueOf key
    return $ case mVal of
        Nothing -> Nothing
        Just val -> 
            let (a, b) = T.breakOn "," val
            in case (T.readMaybe $ T.unpack a, T.readMaybe $ T.unpack (T.tail b)) of
                (Just x, Just y) -> Just (x, y)
                _ -> Nothing
