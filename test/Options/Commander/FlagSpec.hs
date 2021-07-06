module Options.Commander.FlagSpec where

import Test


spec :: Spec
spec = do
  describe "flag" do
    let program :: ProgramT (Flag "flag" & Raw) IO Bool
        program = flag $ raw . pure
    testProgram "has flag"     program (Just True)  ["~flag"]
    testProgram "missing flag" program (Just False) []

