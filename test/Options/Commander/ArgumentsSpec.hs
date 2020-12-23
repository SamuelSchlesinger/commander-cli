module Options.Commander.ArgumentsSpec where

import Test


spec :: Spec
spec = do
  describe "args" do
    it "unrender rest of arguments" do
      let program :: ProgramT (Args "1234" (NonEmpty 2 Int) & Raw) IO (Int,Int,[Int])
          program = args $ \(x :| y:| Tail zs) -> raw $ pure (x,y,zs)
      let state = ["1","2","3","4"]
      let expected = Just (1,2,[3,4])
      result <- runCommanderT (run program) state
      result `shouldBe` expected

    it "consumes all state value" do
      let program :: ProgramT (Args "123" (NonEmpty 0 Int) & Args "None" (NonEmpty 0 Int) & Raw) IO (NonEmpty 0 Int, NonEmpty 0 Int)
          program = args \x -> args \y -> raw $ pure $ (NonEmpty x, NonEmpty y)
      let state = ["1","2","3"]
      let expected = Just (NonEmpty $ Tail [1,2,3], NonEmpty $ Tail [])
      result <- runCommanderT (run program) state
      result `shouldBe` expected

    it "parse fail" do
      let program :: ProgramT (Args "123" (NonEmpty 0 Int) & Raw) IO (NonEmpty 0 Int)
          program = args $ raw . pure . NonEmpty
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

      -- it "NonEmptyTail" do
      --   let program = args

