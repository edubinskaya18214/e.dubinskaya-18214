--1
get1:: [a] -> Int -> a
get1 (x:xs) 0 = x
get1 (x:xs) n = get1 xs (n-1)

--2
head1:: [a] -> a
head1 (x:xs) = x

--3
last1:: [a] -> a
last1 (x:[]) = x
last1 (x:xs) = last1 xs

--4
tail1:: [a] -> [a]
tail1 (x:xs) = xs
tail1 [] = error "empty"

--5
init1 (x:[]) = []
init1 xs = helper xs []
          where helper:: [a] -> [a] -> [a]
                helper (x:[]) a = reverse1 a
                helper (x:tail1') a = helper tail1' (x:a)

--6
reverse1:: [a]-> [a]
reverse1 xs = helper xs []
             where helper [] a = a
                   helper (x:tail1') a = helper tail1' (x:a)

--7
length1:: [a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + length1 xs

--8
append1 :: [a] -> a -> [a]
append1 xs a = append1' xs a 1
              where append1' ys y 1 = append1' (reverse ys) y 2
                    append1' ys y 2 = reverse (y:ys)

--9
--concat1 :: [a] -> [a] -> [a]
concat1 x y = helper x y 1
             where helper [] y _ = y
                   helper xs y 1 = helper (reverse1 xs) y 2
                   helper (x:xs) y 2 = helper xs (x:y) 2
--10
drop1:: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 n [] = []
drop1 n (x:xs) = drop1 (n-1) xs

--11
take1:: Int -> [a] -> [a]
take1 0 _ = []
take1 n (x:xs) = x : take (n-1) xs
                  
--12
splitAt1 :: (Eq t, Num t) => t -> [a] -> [[a]]
splitAt1 0 xs = [[],xs]
splitAt1 n [] = [[],[]]
splitAt1 n (x:xs) = helper n [] (x:xs)
               where helper n xs [] = [[], xs]
                     helper 0 xs ys = [reverse xs, ys]
                     helper n xs (y:ys) = helper (n-1) (y:xs) ys
                     
--13
null1:: [a] -> Bool
null1 [] = True
null1 (x:xs) = False

-- 14
elem1 :: Eq t => t -> [t] -> Bool
elem1 n [] = False
elem1 n (x:xs) = if n==x then True
                 else elem1 n xs
                 
--16
map1 :: (t -> a) -> [t] -> [a]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs


--17 
zip1:: [a1] -> [a] -> [(a1,a)]
zip1 xs ys = helper xs ys []
             where helper [] _ a = reverse1 a
                   helper _ [] a = reverse1 a
                   helper (x:xs) (y:ys) a = helper xs ys ((x,y):a)
