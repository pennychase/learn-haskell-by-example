module Graph where

type DiGraph a = [(a, [a])]

member :: Eq a => a -> [(a, b)] -> Bool
member _ [] = False
member x ((x', _) : xs)
    | x == x' = True
    | otherwise = member x xs

alter :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
alter f key [] =
    case f Nothing of
        Nothing -> []
        Just value -> [(key, value)]
alter f key ((key', value') : xs)
    | key == key' = 
        case f (Just value') of
            Nothing -> xs
            Just value -> (key, value) : xs
    | otherwise = (key', value') : alter f key xs



hasNode :: Eq a => DiGraph a -> a -> Bool
hasNode = flip member

addNode :: Eq a => DiGraph a -> a -> DiGraph a
addNode graph node
    | graph `hasNode` node = graph
    | otherwise = (node, []) : graph