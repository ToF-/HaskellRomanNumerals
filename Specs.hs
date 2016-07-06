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
