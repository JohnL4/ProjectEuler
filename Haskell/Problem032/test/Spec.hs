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
    it "867 --> 867" $ do
      smallestUniqueGE 867 `shouldBe` Just 867
    it "868 --> 869" $ do
      smallestUniqueGE 868 `shouldBe` Just 869
    it "888 --> 891" $ do
      smallestUniqueGE 888 `shouldBe` Just 891
    it "999 --> 1234" $ do
      smallestUniqueGE 999 `shouldBe` Just 1234
    it "777 --> 789" $ do
      smallestUniqueGE 777 `shouldBe` Just 789
    it "199 --> 213" $ do
      smallestUniqueGE 199 `shouldBe` Just 213
      
  describe "flatten1" $ do
    it "returns [1,2,3] for [[1,2,3]]" $ do
      flatten1 [[1,2,3]] `shouldBe` [1,2,3]
    it "returns [4,5,6] for [[4],[5],[6]]" $ do
      flatten1 [[4],[5],[6]] `shouldBe` [4,5,6]

  describe "candidatesGreaterThanOrEqualTo" $ do
    it "first 3 of 1 --> [1,2,3]" $ do
      take 3 (candidatesGreaterThanOrEqualTo 1) `shouldBe` [1,2,3]
    it "first 3 of 9 --> [9, 12, 13]" $ do
      take 3 (candidatesGreaterThanOrEqualTo 9) `shouldBe` [9,12,13]
    it "first 3 of 88 --> [89,91,92]" $ do
      take 3 (candidatesGreaterThanOrEqualTo 88) `shouldBe` [89,91,92]
    it "first 3 of 99 --> [123,124,125]" $ do
      take 3 (candidatesGreaterThanOrEqualTo 99) `shouldBe` [123,124,125]
    it "first 3 of 829 --> [829, 831, 832]" $ do
      take 3 (candidatesGreaterThanOrEqualTo 829) `shouldBe` [829, 831, 832]
    it "first 3 of 329 --> [329, 341, 342]" $ do
      take 3 (candidatesGreaterThanOrEqualTo 329) `shouldBe` [329, 341, 342]
      
