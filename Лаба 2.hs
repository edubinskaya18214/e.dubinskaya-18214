import Data.Char

toDecimal:: Int -> String -> String
toDecimal base snumber = if (base <= 1 || base > 62) then error "Incorrect base" 
                          else if any (\x -> (toDec x) >= base) snumber then error "Wrong number" 
                          else show (foldl (\res x-> base*res + toDec x) 0 snumber)
   where
   toDec a = if a >= '0' && a <= '9' then (ord a - 48)
            else if a >= 'a' && a <= 'z' then (ord a - 87)
            else (ord a - 65 + 36)

----------------------------------------------------------------------------------------------
fromDecimal:: Int -> String -> String
fromDecimal 1 num = if any (\x -> x > '9' || x < '0') num then error "Wrong number"
                    else toOne (read num::Int) []
                     where 
                      toOne:: Int -> String -> String
                      toOne 0 a = a
                      toOne num a = toOne (num-1) ('1':a)
fromDecimal toBase snumber = if any (\x -> x > '9' || x < '0') snumber then error "Wrong number"
                             else if toBase > 61 || toBase < 1 then error "Wrong Base" 
                             else toOtherBase toBase (read snumber::Int) []
                                   where 
                                    toOtherBase:: Int -> Int -> String -> String
                                    toOtherBase b 0 a = a
                                    toOtherBase b num a = toOtherBase b (div num b) ((toEnum(godHelp (mod num b) )::Char):a)
                                    godHelp x = if (x <= 9) then x + 48
                                    else if (x <= 35) then x + 97 - 10
                                    else x + 65 - 36
------------------------------------------------------------------------------------------------

convertFromTo:: Int -> Int -> [Char] -> [Char]
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)
