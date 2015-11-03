-- | Unit test for PassphraseGenerator functions.

import Test.Hspec
import qualified Data.Map.Strict as Map

import PassphraseGenerator

filetext :: String
filetext = "aaa\t1900\t2\naaa\t1950\t3\nbbb\t1950\t5\nbbb_VERB\t1980\t9"

y :: Int
y = 1950

main :: IO()
main = hspec $ do
  describe "countDescending" $ do
    it "returns LT when the first tuple should come before the second tuple" $
      (countDescending ("aaa", 9) ("bbb", 6)) `shouldBe` LT
    it "returns GT when first tuple comes after second tuple" $
      (countDescending ("aaa", 3) ("bbb", 6)) `shouldBe` GT
    it "returns EQ when no order can be determined between tuples" $
      (countDescending ("aaa", 6) ("bbb", 6)) `shouldBe` EQ

  describe "wordCounts" $ do
    it "aaa should occur 3 times" $ do
      (Map.lookup "aaa" (wordCounts y (lines filetext) Map.empty)) `shouldBe` (Just 3)
    it "bbb should occur 14 times" $ do
      (Map.lookup "bbb" (wordCounts y (lines filetext) Map.empty)) `shouldBe` (Just 14)
