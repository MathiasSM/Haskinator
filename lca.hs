lca :: Eq a => [a] -> [a] -> Maybe a
lca [] _ = Nothing
lca _ [] = Nothing

lca (x:xs) (y:ys) 
    | x == y && (take 1 xs) == (take 1 ys)                = lca (xs) (ys)
    | x == y && (take 1 xs) /= (take 1 ys)                = Just x
    | otherwise                                           = Nothing
