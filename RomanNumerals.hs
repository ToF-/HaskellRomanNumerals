module RomanNumerals where

toRoman :: Int -> String
toRoman 0         = ""
toRoman n | n < 4 = replicate n 'I'
toRoman 4         = "IV"
toRoman n | n < 9 = "V" ++ toRoman (n - 5)
