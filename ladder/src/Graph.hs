module Graph where

import qualified Data.AssocMap as M
import qualified Data.List as L

type DiGraph a = M.AssocMap a [a]


empty :: DiGraph a
empty = M.empty

addEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
    where
        insertEdge Nothing = Just [child]
        insertEdge (Just nodes) = Just (L.nub (child : nodes))

addEdges :: (Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges edges graph = foldr addEdge graph edges

buildDiGraph :: (Eq a) => [(a, [a])] -> DiGraph a
buildDiGraph = foldr (\(k, v) xs -> M.insert k v xs) M.empty

children :: (Eq a) => a -> DiGraph a -> [a]
children = M.findWithDefault []

deleteNode :: (Eq a) => a -> DiGraph a -> DiGraph a
deleteNode = M.delete

deleteEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
deleteEdge (node, node') graph = M.alter deleteEdge' node graph
    where
        deleteEdge' Nothing = Nothing
        deleteEdge' (Just nodes) = Just (L.delete node' nodes)