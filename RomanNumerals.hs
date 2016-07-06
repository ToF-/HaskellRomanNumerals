module RomanNumerals where

toRoman :: (String, Int) -> (String, Int)
toRoman (s,0)  = (s,0)
toRoman (s,n) | n < 4 = parseRoman (s,n) 1 "I"
toRoman (s,n) | n < 5 = parseRoman (s,n) 4 "IV"
toRoman (s,n) | n < 9 = parseRoman (s,n) 5 "V"
toRoman (s,n) | n <10 = parseRoman (s,n) 9 "IX"
toRoman (s,n) | n <40 = parseRoman (s,n) 10 "X"
toRoman (s,n) | n <50 = parseRoman (s,n) 40 "XL"
toRoman (s,n) | n <90 = parseRoman (s,n) 50 "L"
toRoman (s,n) | n <100= parseRoman (s,n) 90 "XC"
toRoman (s,n)         = parseRoman (s,n) 100 "C"


parseRoman (r,n) m s = toRoman
    (r ++ concat (replicate (n `div` m) s), n `mod` m)
