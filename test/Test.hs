module Test
  ( testProgram
  , module Options.Commander
  , module Test.Hspec
  ) where

import Options.Commander
import Control.Monad.Commander (runCommanderT)
import Test.Hspec hiding (Arg)


testProgram :: forall a p. (HasProgram p, Show a, Eq a) => String -> ProgramT p IO a -> Maybe a -> State -> Spec
testProgram testName program result state = it testName $ runCommanderT (run program) state >>= (`shouldBe` result)

