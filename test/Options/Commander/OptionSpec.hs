module Options.Commander.OptionSpec where

import Test
import Data.List.NonEmpty


spec :: Spec
spec = do
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

  describe "optDefMulti" do
    describe "string" do
      let program :: ProgramT (Opt '["o","another"] "opt" ('Just "Default") String & Raw) IO String
          program = $(optDefMulti @String ("o" :| ["another"]) "opt" "Default") \o -> raw $ pure o
      testProgram "default"                program (Just "Default" :: Maybe String) []
      testProgram "option first provided"  program (Just "hello")   ["o","hello"]
      testProgram "option second provided" program (Just "there")   ["another","there"]

    describe "works without type signiture" do
      let program = $(optDefMulti @Int ("o" :| ["another"]) "opt" "22") \o -> raw $ pure o -- do not add type signiture
      testProgram "default"                program (Just 22) []
      testProgram "option second provided" program (Just 33) ["o","33"]
      testProgram "option second provided" program (Just 44) ["another","44"]

