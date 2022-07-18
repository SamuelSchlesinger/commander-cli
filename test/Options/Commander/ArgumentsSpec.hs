{-# LANGUAGE LiberalTypeSynonyms #-}
module Options.Commander.ArgumentsSpec where

import Test


spec :: Spec
spec = do
  describe "args" do
    it "unrender rest of arguments" do
      let program = args @"1234 name" \(x :< y :< Tail zs :: AtLeast [] 2 Int) -> raw $ pure (x,y,zs)
      let state = ["1","2","3","4"]
      let expected = Just (1,2,[3,4])
      result <- runCommanderT (run program) state
      result `shouldBe` expected

    it "consumes all state value" do
      let program = args @"consumes all" \x -> args @"consumes none" \y -> raw $ pure $ (x,y)
      let state = ["1","2","3"]
      let expected = Just ([1,2,3],[]) :: Maybe ([Int],[Int])
      result <- runCommanderT (run program) state
      result `shouldBe` expected

    it "parse fail" do
      let program :: ProgramT (Args "123" (AtLeast [] 0 Int) & Raw) IO (AtLeast [] 0 Int)
          program = args $ raw . pure
      let state = ["1","2","3abc"]
      let expected = Nothing
      result <- runCommanderT (run program) state
      result `shouldBe` expected

    describe "works with type" do
      it "[]" do
        let program = args @"[] type" @[] @Int $ raw . pure
        let state = ["1","2","3"]
        let expected = Just [1,2,3]
        result <- runCommanderT (run program) state
        result `shouldBe` expected

      -- it "AtLeastTail" do
      --   let program = args @"AtLeastTail type" $ raw . pure
      --   let state = ["1","2","3"]
      --   let expected = Just $ Head [1] :> 2 :> 3 :: Maybe (AtLeastTail 2 Int)
      --   result <- runCommanderT (run program) state
      --   result `shouldBe` expected

  describe "argsN" do
    it "parses exactly n argumnets" do
      let program = argsN @"123" @3 @Int \x -> raw $ pure x
      let state = ["1","2","3","4"]
      let expected = Just $ 1 :< 2 :< 3 :< Tail Proxy
      result <- runCommanderT (run program) state
      result `shouldBe` expected

    it "leaves remaining values in state" do
      let program :: ProgramT (Args "consumer" (Exact 2 Int) & Args "remaining" [Int] & Raw) IO [Int]
          program = argsN @"consumer" @2 \(_ :< _ :< Tail Proxy) -> args $ raw . pure
      let state = ["1","2","3","4"]
      let expected = Just [3,4]
      result <- runCommanderT (run program) state
      result `shouldBe` expected

    it "not enough arguments" do
      let program = argsN @"Nothing" @3 @Int \x -> raw $ pure x
      let state = ["1","2"]
      let expected = Nothing
      result <- runCommanderT (run program) state
      result `shouldBe` expected

  it "argsButN" do
    let program = argsButN @"all but" @2 \(x :: [Int]) -> args @"last args" \(y :: [Int]) -> raw $ pure (x,y)
    let state = ["1","2","3","4","5"]
    let expected = Just ([1,2],[3,4,5])
    result <- runCommanderT (run program) state
    result `shouldBe` expected

