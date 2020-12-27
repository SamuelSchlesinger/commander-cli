{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Options.Commander.Arguments.AtLeastSpec where

import Test
import Data.Foldable
import Data.Functor.Identity


spec :: Spec
spec = do
  describe "AtLeast" do
    describe "FromList" do
      it "has enough" $ fromList [1,2,3,4] `shouldBe` (Just $ 1 :< 2 :< Tail [3,4])
      it "not enough" $ fromList [1,2] `shouldBe` (Nothing :: Maybe (AtLeast 3 Int))

    describe "Foldable" do
      it "toList" $ toList (1 :< 2 :< Tail [3,4]) `shouldBe` [1,2,3,4]
      it "foldr" $ foldr (:) [5] (1 :< 2 :< Tail [3,4]) `shouldBe` [1,2,3,4,5]

  describe "AtLeastTail" do
    describe "FromList" do
      it "has enough" $ fromList [1,2,3,4] `shouldBe` (Just $ Head [1,2] :> 3 :> 4)
      it "not enough" $ fromList [1,2] `shouldBe` (Nothing :: Maybe (AtLeastTail 3 Int))

    describe "Foldable" do
      it "toList" $ toList (Head [1,2] :> 3 :> 4) `shouldBe` [1,2,3,4]
      it "foldr" $ foldr (:) [5] (Head [1,2] :> 3 :> 4) `shouldBe` [1,2,3,4,5]

    it "Traversable" do
      let expected :: AtLeastTail 2 Int
          expected = Head [1,2] :> 3 :> 4
      let result = runIdentity $ traverse Identity expected
      result `shouldBe` expected

  describe "Exactly" do
    describe "FromList" do
      it "has correct number of elements" $
        fromList [1,2,3] `shouldBe` Just (1 :< 2 :< 3 :< Tail Proxy)

      it "to few elements" $
        fromList [1,2] `shouldBe` (Nothing :: Maybe (Exact 3 Int))

