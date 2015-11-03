-- | Unit test for PassphraseGenerator functions.

import Test.Hspec
import PassphraseGenerator

main :: IO()
main = hspec $ do
  describe "countDescending" $ do
    it "returns LT when the first tuple should come before the second tuple" $
      (countDescending ("aaa", 9) ("bbb", 6)) `shouldBe` LT
      
    
