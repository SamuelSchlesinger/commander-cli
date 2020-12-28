module Options.Commander.OptionSpec where

import Test


spec :: Spec
spec = do
  describe "opt" do
    let program :: forall a. Unrender a => ProgramT (Opt "opt" "opt" a & Raw) IO (Maybe a)
        program = opt \o -> raw $ pure o
    testProgram "string option"   program (Just $ Just "hello" :: Maybe (Maybe String)) ["-opt","hello"]
    testProgram "int option"      program (Just $ Just 2       :: Maybe (Maybe Int))    ["-opt","2"]
    testProgram "unrender error"  program (Nothing             :: Maybe (Maybe Int))    ["-opt","abc"]
    testProgram "missing  value"  program (Just Nothing        :: Maybe (Maybe String)) ["-opt"]
    testProgram "missing  option" program (Just Nothing        :: Maybe (Maybe String)) ["abc"]

  describe "optDef" do
    let program :: ProgramT (Opt "o" "opt" String & Raw) IO String
        program = optDef "Default" \o -> raw $ pure o
    testProgram "default option"  program (Just "Default") []
    testProgram "option provided" program (Just "hello")   ["-o","hello"]

