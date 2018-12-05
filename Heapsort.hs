heapsort:: Ord a => [a] -> [a]
heapsort (x:[]) = [x]
heapsort arr = ( head ( largest' arr )):(heapsort (tail ( largest' arr )))
 where
  left arr = arr!!(rightPoint arr - 1)
  right arr = arr!!(rightPoint arr)
  rightPoint arr = 2*div (length arr) 2 + 0

  beforeLeft arr = if rightPoint arr >= length arr then ( init( drop  (div (length arr) 2) arr) ) else init ( init( drop  (div (length arr) 2) arr) )
  beforePoint arr = take (div (length arr) 2 - 1) arr
  point arr = arr!!(div (length arr) 2 - 1)
  replase arr = if rightPoint arr >= length arr then replase2 arr
                else replase3 arr

  replase2 arr = if ( point arr > (left arr)) then arr 
                 else (beforePoint arr)++[(left arr)]++(beforeLeft arr)++[(point arr)]
  replase3:: Ord a => [a]->[a]
  replase3 arr = if (point arr > left arr && point arr > right arr) then arr
                 else if left arr > point arr && left arr > right arr then (beforePoint arr)++[(left arr)]++(beforeLeft arr)++[(point arr)]++[(right arr)]
                 else (beforePoint arr)++[(right arr)]++(beforeLeft arr)++[(point arr)]++[(left arr)]
  largest' arr = largest arr (length arr - 2)
  largest x 0 = x
  largest arr n = (largest (init (replase arr)) (n-1))++[last (replase arr) ]
