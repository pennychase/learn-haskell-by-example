{-# LANGUAGE ScopedTypeVariables #-}

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

deleteNodes :: (Eq a) => [a] -> DiGraph a -> DiGraph a
deleteNodes [] graph = graph
deleteNodes (x : xs ) graph = M.delete x (deleteNodes xs graph)

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

-- Breadth First Search

type SearchState a =([a], DiGraph a, DiGraph a)  -- frontier, graph, predecessors graph

data SearchResult a = Unsuccessful | Successful (DiGraph a)

bfsSearch :: forall a. Eq a => DiGraph a -> a -> a -> Maybe [a]
bfsSearch graph start end
    | start == end = Just [start]
    | otherwise =
        case bfsSearch' ([start], graph, empty) of
            Successful preds -> Just (findSolution preds)
            Unsuccessful -> Nothing

    where
        addMultiplePredecessors :: [(a, [a])] -> DiGraph a -> DiGraph a
        addMultiplePredecessors [] g = g
        addMultiplePredecessors ((n, ch) : xs) g =
            addMultiplePredecessors xs (go n ch g)
            where
                go n [] g = g
                go n (x : xs) g = go n xs (addEdge (x, n) g)

        bfsSearch' :: SearchState a -> SearchResult a
        bfsSearch' ([], _, preds) = Unsuccessful
        bfsSearch' (frontier, g, preds) =
            let
                g' = deleteNodes frontier g
                ch = L.map (\n -> (n, L.filter (`M.member` g) (children n g))) frontier
                frontier' = L.concatMap snd ch
                preds' = addMultiplePredecessors ch preds
            in if end `L.elem` frontier'
                then Successful preds'
                else bfsSearch' (frontier', g', preds')

        findSolution :: DiGraph a -> [a]
        findSolution g = L.reverse (go end)
            where
                go x =
                    case children x g of
                        [] -> [x]
                        (v : _) -> x : go v