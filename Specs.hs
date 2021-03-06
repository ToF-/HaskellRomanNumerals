import Test.Hspec
import RomanNumerals

shouldConvertTo n r = do
    it ("should convert " ++ (show n) ++ " to " ++ r) $ do
        toRoman n `shouldBe` r

main = hspec $ do
    describe "toRoman" $ do
        1 `shouldConvertTo` "I"
        3 `shouldConvertTo` "III"
        4 `shouldConvertTo` "IV"
        5 `shouldConvertTo` "V"
        8 `shouldConvertTo` "VIII"
        9 `shouldConvertTo` "IX"
        30 `shouldConvertTo` "XXX"
        34 `shouldConvertTo` "XXXIV"
        222 `shouldConvertTo` "CCXXII"
        46 `shouldConvertTo` "XLVI"
        62 `shouldConvertTo` "LXII"
        93 `shouldConvertTo` "XCIII"
        3952 `shouldConvertTo` "MMMCMLII"
        645 `shouldConvertTo` "DCXLV"
        401 `shouldConvertTo` "CDI"
