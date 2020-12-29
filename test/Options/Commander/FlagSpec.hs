module Options.Commander.FlagSpec where

import Test


spec :: Spec
spec = do
  describe "flag" do
    let program :: ProgramT (Flag '["flag"] & Raw) IO Bool
        program = flag $ raw . pure
    testProgram "has flag"     program (Just True)  ["flag"]
    testProgram "missing flag" program (Just False) []

  describe "flagMulti" do
    let program :: ProgramT (Flag '["flag","another"] & Raw) IO Bool
        program = flagMulti $ raw . pure
    testProgram "has first flag"  program (Just True)  ["flag"]
    testProgram "has second flag" program (Just True)  ["another"]
    testProgram "missing flag"    program (Just False) []

