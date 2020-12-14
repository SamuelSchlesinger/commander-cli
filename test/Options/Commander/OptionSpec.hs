module Options.Commander.OptionSpec where

import Test


spec :: Spec
spec = do
  describe "opt" do
    let program :: forall a. Unrender a => ProgramT (Opt '["opt"] "opt" a & Raw) IO (Maybe a)
        program = opt \o -> raw $ pure o
    testProgram "string option"   program (Just $ Just "hello" :: Maybe (Maybe String)) ["opt","hello"]
    testProgram "int option"      program (Just $ Just 2       :: Maybe (Maybe Int))    ["opt","2"]
    testProgram "unrender error"  program (Nothing             :: Maybe (Maybe Int))    ["opt","abc"]
    testProgram "missing  value"  program (Just Nothing        :: Maybe (Maybe String)) ["opt"]
    testProgram "missing  option" program (Just Nothing        :: Maybe (Maybe String)) ["abc"]

  describe "optMulti" do
    let program :: forall a. Unrender a => ProgramT (Opt '["opt","another"] "opt" a & Raw) IO (Maybe a)
        program = optMulti \o -> raw $ pure o
    testProgram "string first option"   program (Just $ Just "hello" :: Maybe (Maybe String)) ["opt","hello"]
    testProgram "int second option"     program (Just $ Just 2       :: Maybe (Maybe Int))    ["another","2"]
    testProgram "unrender first error"  program (Nothing             :: Maybe (Maybe Int))    ["opt","abc"]
    testProgram "unrender second error" program (Nothing             :: Maybe (Maybe Int))    ["another","abc"]
    testProgram "missing first value"   program (Just Nothing        :: Maybe (Maybe String)) ["opt"]
    testProgram "missing second value"  program (Just Nothing        :: Maybe (Maybe String)) ["another"]
    testProgram "missing  option"       program (Just Nothing        :: Maybe (Maybe String)) ["abc"]

  describe "optDef" do
    let program :: ProgramT (Opt '["o"] "opt" String & Raw) IO String
        program = optDef "Default" \o -> raw $ pure o
    testProgram "default option"  program (Just "Default") []
    testProgram "option provided" program (Just "hello")   ["o","hello"]

  describe "optDefMulti" do
    let program :: a -> ProgramT (Opt '["o","another"] "opt" a & Raw) IO a
        program defaultVal = optDefMulti defaultVal \o -> raw $ pure o
    testProgram "default option"         (program ("Default" :: String)) (Just "Default") []
    testProgram "option first provided"  (program ("Default" :: String)) (Just "hello")   ["o","hello"]
    testProgram "option second provided" (program (1         :: Int))    (Just 4)         ["another","4"]

