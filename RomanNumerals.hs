module RomanNumerals where

toRoman :: Int -> String
toRoman 0         = ""
toRoman n | n < 4 = parseRoman n 1 'I'
toRoman 4         = "IV"
toRoman n | n < 9 = "V" ++ toRoman (n - 5)
toRoman 9         = "IX"
toRoman n | n <40 = parseRoman n 10 'X'
toRoman n | n <50 = "XL" ++ toRoman (n - 40)
                  
toRoman n         = parseRoman n 100 'C'


parseRoman n m c = replicate (n `div` m) c
                  ++ toRoman (n `mod` m)
