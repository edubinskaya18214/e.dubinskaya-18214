map1 :: (a -> b) -> [a] -> [b]  
map1 f (xs) = reverse (foldl (\acc x-> f x : acc) [] xs) 