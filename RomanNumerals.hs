module RomanNumerals where

toRoman :: Int -> String
toRoman 0         = ""
toRoman n | n < 4 = parseRoman n 1 'I'
toRoman n | n < 5 = "IV" ++ toRoman (n `mod` 4)
toRoman n | n < 9 = parseRoman n 5 'V'
toRoman n | n <10 = "IX" ++ toRoman (n `mod` 9)
toRoman n | n <40 = parseRoman n 10 'X'
toRoman n | n <50 = "XL" ++ toRoman (n `mod` 40)
toRoman n | n <90 = parseRoman n 50 'L'
toRoman n | n <100= "XC" ++ toRoman (n `mod` 90)        
toRoman n         = parseRoman n 100 'C'


parseRoman n m c = replicate (n `div` m) c
                  ++ toRoman (n `mod` m)
