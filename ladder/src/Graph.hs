module Graph where

import qualified Data.AssocMap as M
import qualified Data.List as L

type DiGraph a = M.AssocMap a [a]

empty :: DiGraph a
empty = M.empty

hasNode :: (Eq a) => DiGraph a -> a -> Bool
hasNode = flip M.member

addNode :: (Eq a) => DiGraph a -> a -> DiGraph a
addNode graph node
    | graph `hasNode` node = graph
    | otherwise = M.insert node [] graph

deleteNode :: (Eq a) => DiGraph a -> a -> DiGraph a
deleteNode graph node = M.delete node graph

addEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
    where
        insertEdge Nothing = Just [child]
        insertEdge (Just nodes) = Just (L.nub (child : nodes))

addEdges :: (Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges edges graph = foldr addEdge graph edges  

deleteEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
deleteEdge (node, child) graph =
    case children node graph of
        [] -> graph
        nodes -> M.insert node (L.delete child nodes) graph

buildDiGraph :: (Eq a) => [(a, [a])] -> DiGraph a
buildDiGraph adjacencyList = foldr addAdjacent empty adjacencyList
    where
        addAdjacent :: (Eq a) => (a, [a]) -> DiGraph a -> DiGraph a
        addAdjacent (node, edges) graph = M.insert node edges graph

children :: (Eq a) => a -> DiGraph a -> [a]
children = M.findWithDefault []