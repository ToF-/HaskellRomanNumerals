module RomanNumerals where

toRoman :: Int -> String
toRoman 0         = ""
toRoman n | n < 4 = replicate (n `div` 1) 'I'
                   ++ toRoman (n `mod` 1) 
toRoman 4         = "IV"
toRoman n | n < 9 = "V" ++ toRoman (n - 5)
toRoman 9         = "IX"
toRoman n | n <40 = replicate (n `div` 10) 'X' 
                   ++ toRoman (n `mod` 10)
                  
toRoman n         = replicate (n `div` 100) 'C' 
                   ++ toRoman (n `mod` 100)
