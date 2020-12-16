module Options.Commander.OptionSpec where

import Test


spec :: Spec
spec = do
  describe "opt" do
    let program :: forall a. Unrender a => ProgramT (Opt '["opt"] "opt" 'Nothing a & Raw) IO (Maybe a)
        program = opt \o -> raw $ pure o
    testProgram "string option"        program (Just $ Just "hello" :: Maybe (Maybe String)) ["opt","hello"]
    testProgram "int option"           program (Just $ Just 2       :: Maybe (Maybe Int))    ["opt","2"]
    testProgram "unrender error"       program (Nothing             :: Maybe (Maybe Int))    ["opt","abc"]
    testProgram "missing  value"       program (Just Nothing        :: Maybe (Maybe String)) ["opt"]
    testProgram "missing  option"      program (Just Nothing        :: Maybe (Maybe String)) ["abc"]
    testProgram "not at head of state" program (Just $ Just 3       :: Maybe (Maybe Int))    ["notopt","opt","3"]

  describe "optMulti" do
    let program :: forall a. Unrender a => ProgramT (Opt '["opt","another"] "opt" 'Nothing a & Raw) IO (Maybe a)
        program = optMulti \o -> raw $ pure o
    testProgram "string first option"   program (Just $ Just "hello" :: Maybe (Maybe String)) ["opt","hello"]
    testProgram "int second option"     program (Just $ Just 2       :: Maybe (Maybe Int))    ["another","2"]
    testProgram "unrender first error"  program (Nothing             :: Maybe (Maybe Int))    ["opt","abc"]
    testProgram "unrender second error" program (Nothing             :: Maybe (Maybe Int))    ["another","abc"]
    testProgram "missing first value"   program (Just Nothing        :: Maybe (Maybe String)) ["opt"]
    testProgram "missing second value"  program (Just Nothing        :: Maybe (Maybe String)) ["another"]
    testProgram "missing  option"       program (Just Nothing        :: Maybe (Maybe String)) ["abc"]

  describe "optDef" do
    describe "String type" do
      let program :: ProgramT (Opt '["o"] "opt" ('Just "Default") String & Raw) IO String
          program = $(optDef @String "o" "opt" "Default") \o -> raw $ pure o
      testProgram "default option"  program (Just "Default") []
      testProgram "option provided" program (Just "hello")   ["o","hello"]

    describe "Int type without signiture" do
      let program = $(optDef @Int "o" "opt" "2") \o -> raw $ pure o
      testProgram "default option"  program (Just 2) []
      testProgram "option provided" program (Just 4)   ["o","4"]

  describe "optDefMulti" do
    let program :: forall def a. ProgramT (Opt '["o","another"] "opt" ('Just def) a & Raw) IO a
        program = optDefMulti \o -> raw $ pure o
    testProgram "default option"         (program @"Default" @String) (Just "Default") []
    testProgram "option first provided"  (program @"Default" @String) (Just "hello")   ["o","hello"]
    testProgram "option second provided" (program @"1"       @Int)    (Just 4)         ["another","4"]

