import Test.Hspec
import RomanNumerals

main = hspec $ do
    describe "toRoman" $ do
        it "should convert 1 to I" $ do
            toRoman 1 `shouldBe` "I"
        it "should convert 3 to III" $ do
            toRoman 3 `shouldBe` "III"
