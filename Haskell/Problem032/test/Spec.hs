import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "numToDigits" $ do
    it "turns a number into a list of digits" $ do
      numToDigits 867 `shouldBe` [8,6,7]
      
  describe "digitsToNum" $ do
    it "turns a sequence of digits into a number" $ do
      digitsToNum [8,6,7] `shouldBe` 867

  describe "smallestUniqueGE" $ do
    it "finds 867 for input 867" $ do
      smallestUniqueGE 867 `shouldBe` 867
    it "finds 869 for 868" $ do
      smallestUniqueGE 868 `shouldBe` 869
    it "finds 912 for input 877" $ do
      smallestUniqueGE 888 `shouldBe` 912
    it "finds 1234 for input 999" $ do
      smallestUniqueGE 999 `shouldBe` 1234
      
  describe "flatten1" $ do
    it "returns [1,2,3] for [[1,2,3]]" $ do
      flatten1 [[1,2,3]] `shouldBe` [1,2,3]
    it "returns [4,5,6] for [[4],[5],[6]]" $ do
      flatten1 [[4],[5],[6]] `shouldBe` [4,5,6]
                
