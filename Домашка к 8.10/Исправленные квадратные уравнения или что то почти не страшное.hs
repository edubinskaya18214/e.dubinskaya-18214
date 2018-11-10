square:: Num a => a -> a -> a -> [a]
square a b c 
          | a == 0 && b == 0 = error "No roots"
          | a == 0 = [-c/b , -c/b]
          |(d a b c <0) = error "No roots"
          |(d a b c ==0) = [-b/(2*a) , -b/ (2*a)]
          |(d a b c >0) = [((- b - sqrt(d a b c))/(2*a)), ((-b +(sqrt (d a b c)))/(2*a))]
      where d a b c= b*b - 4*a*c
