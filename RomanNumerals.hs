module RomanNumerals where

toRoman :: Int -> String
toRoman 0         = ""
toRoman n | n < 4 = parseRoman n 1 "I"
toRoman n | n < 5 = parseRoman n 4 "IV"
toRoman n | n < 9 = parseRoman n 5 "V"
toRoman n | n <10 = parseRoman n 9 "IX"
toRoman n | n <40 = parseRoman n 10 "X"
toRoman n | n <50 = parseRoman n 40 "XL"
toRoman n | n <90 = parseRoman n 50 "L"
toRoman n | n <100= parseRoman n 90 "XC"
toRoman n         = parseRoman n 100 "C"


parseRoman n m s = concat (replicate (n `div` m) s)
                  ++ toRoman (n `mod` m)
