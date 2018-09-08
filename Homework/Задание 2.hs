fun x | x >= 2    = logBase 10 (x^2 - 4)
      | x <= (-2)    = logBase 10 (x^2 - 4)
      | otherwise = error "No"

