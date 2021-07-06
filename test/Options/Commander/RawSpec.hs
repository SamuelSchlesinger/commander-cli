module Options.Commander.RawSpec where

import Test

spec :: Spec
spec = do
  let program :: ProgramT Raw IO String
      program = raw $ pure "expeced-value"
  testProgram "correct returned value" program (Just "expeced-value") []

