module RomanNumerals where

toRoman :: Int -> String
toRoman n | n < 4 = replicate n 'I'
toRoman 4         = "IV"
toRoman 5         = "V"
