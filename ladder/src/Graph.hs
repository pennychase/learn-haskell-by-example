module Graph where

import qualified Data.AssocMap as M

type DiGraph a = M.AssocMap a [a]

empty :: DiGraph a
empty = M.empty

hasNode :: Eq a => DiGraph a -> a -> Bool
hasNode = flip M.member

addNode :: Eq a => DiGraph a -> a -> DiGraph a
addNode graph node
    | graph `hasNode` node = graph
    | otherwise = M.insert node [] graph