module Main where

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Word (Word64)
import Data.Text
import Control.Concurrent
import Options.Commander
import Control.Monad
import System.Exit
import Control.Exception
import System.Environment (setEnv)
import Data.Maybe
import Test.Hspec hiding (Arg)


test :: forall a p. (HasProgram p, Show a, Eq a) => String -> ProgramT p IO a -> Maybe a -> State -> Spec
test testName program result state = it testName $ runCommanderT (run program) state >>= (`shouldBe` result)

main :: IO ()
main = hspec do
  describe "raw" do
    let program :: ProgramT Raw IO String
        program = raw $ pure "expeced-value"
    test "correct returned value" program (Just "expeced-value") []

  describe "arg" do
    let program :: forall a. Unrender a => ProgramT (Arg "arg" a & Raw) IO a
        program = arg \a -> raw $ pure a
    test "string arg"     program (Just "hello" :: Maybe String) ["hello"]
    test "int arg"        program (Just 1       :: Maybe Int)    ["1"]
    test "unrender error" program (Nothing      :: Maybe Int)    ["abc"]
    test "empty state"    program (Nothing      :: Maybe String) []

  describe "option" do
    describe "opt" do
      let program :: forall a. Unrender a => ProgramT (Opt "opt" "opt" a & Raw) IO (Maybe a)
          program = opt \o -> raw $ pure o
      test "string option"   program (Just $ Just "hello" :: Maybe (Maybe String)) ["opt","hello"]
      test "int option"      program (Just $ Just 2       :: Maybe (Maybe Int))    ["opt","2"]
      test "unrender error"  program (Nothing             :: Maybe (Maybe Int))    ["opt","abc"]
      test "missing  value"  program (Just Nothing        :: Maybe (Maybe String)) ["opt"]
      test "missing  option" program (Just Nothing        :: Maybe (Maybe String)) ["abc"]

    describe "optDef" do
      let program :: ProgramT (Opt "o" "opt" String & Raw) IO String
          program = optDef "Default" \o -> raw $ pure o
      test "default option"  program (Just "Default") []
      test "option provided" program (Just "hello")   ["o","hello"]

  describe "flag" do
    let program :: ProgramT (Flag "flag" & Raw) IO Bool
        program = flag $ raw . pure
    test "has flag" program (Just True)  ["flag"]
    test "no flag"  program (Just False) []

  describe "env" do
    it "env" do
      let program :: forall a. Unrender a => ProgramT (Env 'Required "ONOGOTTAFINDABIGNAME" a & Raw) IO a
          program = env \e -> raw $ pure e
      let test result = runCommanderT (run program) [] >>= (`shouldBe` result)
      test (Nothing     :: Maybe String)
      setEnv "ONOGOTTAFINDABIGNAME" "POOP"
      test (Just "POOP" :: Maybe String)
      setEnv "ONOGOTTAFINDABIGNAME" "6"
      test (Just 6      :: Maybe Int)

    it "envOpt" do
      let program :: forall a. ProgramT (Env 'Optional "BIGNAME" a & Raw) IO (Maybe a)
          program = envOpt \e -> raw $ pure e
      let test result = runCommanderT (run $ program) [] >>= (`shouldBe` result)
      test $ Just $ (Nothing    :: Maybe String)
      setEnv "BIGNAME" "POP"
      test $ Just $ (Just "POP" :: Maybe String)
      setEnv "BIGNAME" "3"
      test $ Just $ (Just 3     :: Maybe Int)

    it "envOptDef" do
      let program :: a -> ProgramT (Env 'Optional "CORPUS" a & Raw) IO a
          program x = envOptDef x \e -> raw $ pure e
      let test defaultVal result = runCommanderT (run $ program defaultVal) [] >>= (`shouldBe` result)
      test (1      :: Int)    $ Just 1
      setEnv "CORPUS" "2"
      test (1      :: Int)    $ Just 2
      setEnv "CORPUS" "POOP"
      test ("POOP" :: String) $ Just "POOP"

  describe "sub" do
    let program ::  ProgramT ("a" & Raw + "b" & Raw) IO (Either Int String)
        program = (sub $ raw $ pure $ Left 1) <+> (sub $ raw $ pure $ Right "abc")
    test "first  sub program" program (Just $ Left 1)      ["a"]
    test "second sub program" program (Just $ Right "abc") ["b"]
    test "wrong  sub program" program Nothing              ["c"]
    test "no     sub program" program Nothing              []

  describe "bigProgTests" do
    let program
          :: ProgramT
              ( "argument" & Arg "arg" String & Flag "flag" & Raw
              + Opt "opt" "option-test" Word64 & "option" & Raw )
              IO
              (Either (String,Bool) (Maybe Word64))
        program = (sub @"argument" $ arg $ \a -> flag $ \f -> raw $ pure $ Left (a,f)) <+>
                  (opt \o -> sub @"option" $ raw $ pure $ Right o)
    let test result state = it (show state) $ runCommanderT (run program) state >>= (`shouldBe` result)
    test (Just $ Left ("arg",True)) ["argument","arg","flag"]
    test (Just $ Right $ Just 0)    ["option","opt","0"]
    test Nothing                    ["argument"]
    test Nothing                    ["argument","opt","option"]
    test Nothing                    ["argument","opt","option","flag"]
    test (Just $ Right $ Just 0)    ["option","opt'","option"]
    test (Just $ Right $ Just 1)    ["option","opt","1","flag"]

  describe "eitherSwitch" do
    let program :: ProgramT (Arg "xy" (Either Int String) & Raw) IO (Either Int String)
        program = arg \case
          Left  x -> raw $ pure $ Left  x
          Right y -> raw $ pure $ Right y
    test "left"         program (Just $ Left 10)       ["10"]
    test "right"        program (Just $ Right "Hello") ["Hello"]
    test "no arguments" program Nothing                []

