module Data.AssocMap
    ( AssocMap,
      genAssocMap,
      shrinkAssocMap,
      empty,
      member,
      alter,
      lookup,
      findWithDefault,
      delete,
      insert,
      prop_lookup,
      prop_insertTwice,
      prop_delete,
      prop_empty,
      prop_findWithDefault
    )
    where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import Test.QuickCheck

newtype AssocMap k v = AssocMap [(k, v)]
    deriving (Show)

empty :: AssocMap k v
empty = AssocMap []

delete :: (Eq k) => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: (Eq k) => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter f key (AssocMap xs) = AssocMap (alter' f key xs)
    where
        alter' :: (Eq k) => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
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

member :: (Eq k) => k -> AssocMap k v -> Bool
member key (AssocMap xs) = member' key xs
    where
        member' :: (Eq k) => k -> [(k, v)] -> Bool
        member' _ [] = False
        member' x ((x', _) : xs)
            | x == x' = True
            | otherwise = member' x xs

lookup :: (Eq k) => k -> AssocMap k v -> Maybe v
lookup key (AssocMap xs) = lookup' key xs
    where
        lookup' :: (Eq k) => k -> [(k, v)] -> Maybe v
        lookup' _ [] = Nothing
        lookup' key ((key', value) : xs)
            | key == key' = Just value
            | otherwise = lookup' key xs

findWithDefault :: (Eq k) => v -> k -> AssocMap k v -> v
findWithDefault defaultValue key map =
    fromMaybe defaultValue (lookup key map)

-- Property Tests

-- Create an (Assoc k v) instance of Arbitrary
genAssocMap :: (Eq k, Arbitrary k, Arbitrary v) => Gen (AssocMap k v)
genAssocMap = do
    keys <- L.nub <$> arbitrary
    vals <- vectorOf (L.length keys) arbitrary
    return $ AssocMap (L.zip keys vals)

shrinkAssocMap :: (Eq k, Arbitrary k, Arbitrary v) => AssocMap k v -> [AssocMap k v]
shrinkAssocMap (AssocMap xs) =
    L.map (AssocMap . L.nubBy (\(k1, _) (k2, _) -> k1 == k2)) (shrink xs)

instance (Eq k, Arbitrary k, Arbitrary v) => Arbitrary (AssocMap k v)
    where
        arbitrary = genAssocMap
        shrink = shrinkAssocMap

prop_lookup :: AssocMap Int Int -> Int -> Int -> Property
prop_lookup am k v = label' $ lookup k (insert k v am) == Just v
    where
        label' =
          label
            ( if lookup k am == Nothing
                then "Key not present before insertion"
                else "Key present before insertion"
            )

prop_insertTwice :: AssocMap Int Int -> Int -> Int -> Int -> Property
prop_insertTwice am k v1 v2 = 
    v1 /= v2 ==>    -- v1 is not equal to v2 for the test to run
        label' $ lookup k (insert k v2 (insert k v1 am)) == Just v2
            where
                label' =
                  label
                    ( if lookup k am == Nothing
                        then "Key not present before insertion"
                        else "Key present before insertion"
                    )

-- Use an Assoc Bool Int so there are just two values for keys, making it more probable that 
-- the key occurs
prop_delete :: AssocMap Bool Int -> Bool -> Property 
prop_delete am k = label' $ lookup k (delete k am) == Nothing
    where
        label' =
          label
            ( if lookup k am == Nothing
                then "Key not present before deletion"
                else "Key present before deletion"
            )

prop_empty ::  Int -> Property
prop_empty k = withNumTests 10000 $ not (member k empty)

prop_findWithDefault :: AssocMap Int Int -> Int -> Int -> Property
prop_findWithDefault am defaultValue k = label' $ findWithDefault defaultValue k am == value
    where
        value = case (lookup k am) of
            Nothing -> defaultValue
            Just v -> v
        label' =
            label
                ( if lookup k am == Nothing
                    then "Key not present before findWithDefault"
                    else "Key present before findWithDefault"
                )

