module RomanNumerals where

toRoman :: Int -> String
toRoman n = fst $ foldl parseRoman ("", n) divisors

divisors :: [(Int, String)]
divisors =[(1000,"M")
          ,(900, "CM")
          ,(500, "D")
          ,(400, "CD")
          ,(100, "C")
          ,(90 , "XC")
          ,(50 , "L")
          ,(40 , "XL")
          ,(10 , "X")
          ,(9 ,  "IX")
          ,(5 ,  "V")
          ,(4 ,  "IV")
          ,(1 ,  "I")
          ]


parseRoman :: (String, Int) -> (Int,String) -> (String, Int)
parseRoman (r,n) (m,s) =
    (r ++ concat (replicate (n `div` m) s), n `mod` m)
