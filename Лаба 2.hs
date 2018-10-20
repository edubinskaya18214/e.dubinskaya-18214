toDecimal base snumber = helper1 base snumber 0 1
 where 
  helper1 0 a chislo k = error "Ti sovsem durak?"
  helper1 b a chislo k = helper2 b (reverse a) chislo k
  helper2 b [] chislo k = show chislo
  helper2 b (x:xs) chislo k = helper2 b xs (chislo + ((number x) * k)) (k*b)
   where
    number a = if a >= '0' && a <= '9' then (fromEnum a - 48)
               else if a >= 'a' && a <= 'z' then (fromEnum a - 87)
               else (fromEnum a - 65 + 36)

-----------------------------------------------------------------------------------------------
fromDecimal toBase snumber = helper2 toBase snumber 0 
 where 
  helper2 b ['0'] chislo = snumber;
  helper2 b [] chislo = toOtherBase b chislo []
  helper2 b (x:xs) chislo = if (b > 61 || b < 2) then error "Ti sovsem durak?"
                            else if ( x > '9' || x < '0') then error "Ti sovsem durak?"
                            else helper2 b xs (((fromEnum x) - 48) + chislo*10)
                            
  toOtherBase:: Int -> Int -> [Char] -> [Char]
  toOtherBase b 0 a = a
  toOtherBase b chislo a = toOtherBase b (div chislo b) ((toEnum(godHelp (mod chislo b) )::Char):a)
   where
   godHelp x = if (x <= 9) then x + 48
               else if (x <= 35) then x + 97 - 10
               else x + 65 - 36
------------------------------------------------------------------------------------------------

convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)