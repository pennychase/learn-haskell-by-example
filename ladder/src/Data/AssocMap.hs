module Data.AssocMap
    ( AssocMap,
      alter,
      delete,
      empty,
      findWithDefault,
      insert,
      lookup,
      member
    )
    where

import Prelude hiding (lookup)

newtype AssocMap k v = AssocMap [(k, v)]
    deriving Show

empty :: AssocMap k v
empty = AssocMap []

member :: Eq k => k -> AssocMap k v -> Bool
member key (AssocMap xs) = member' key xs
    where
        member' _ [] = False
        member' x ((x', _) : xs)
            | x == x' = True
            | otherwise = member' x xs

lookup :: Eq k => k -> AssocMap k v -> Maybe v
lookup key (AssocMap xs) = lookup' key xs
    where
        lookup' _ [] = Nothing
        lookup' key ((key', value') : xs) 
            | key == key' = Just value'
            | otherwise = lookup' key xs


findWithDefault :: Eq k => v -> k -> AssocMap k v -> v
findWithDefault defaultValue key map =
    case lookup key map of 
        Nothing -> defaultValue
        Just value -> value


alter :: Eq k => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter f key (AssocMap xs) = AssocMap $ alter' f key xs
    where
        alter' f key [] =
            case f Nothing of
                Nothing -> []
                Just value -> [(key, value)]
        alter' f key ((key', value') : xs)
            | key == key' = 
                case f (Just value') of
                    Nothing -> xs
                    Just value -> (key, value) : xs
            | otherwise = (key', value') : alter' f key xs

delete :: Eq k => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: Eq k => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

