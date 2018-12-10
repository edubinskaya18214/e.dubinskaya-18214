map1 :: (a -> b) -> [a] -> [b]  
map1 f (xs) = reverse (foldl (\acc x-> f x : acc) [] xs) 

map2 :: Foldable t => (t1 -> a) -> t t1 -> [a]
map2 f (xs) = foldr (\x acc-> f x:acc) [] xs
