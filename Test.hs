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


main :: IO ()
main = hspec do
  it "raw" do
    let rawProg :: ProgramT Raw IO Bool
        rawProg = raw (pure True)
    x <- runCommanderT (run rawProg) []
    x `shouldBe` Just True

  it "arg" do
    let argProg :: (String -> Bool) -> ProgramT (Arg "arg" String & Raw) IO Bool
        argProg prop = arg \a -> raw (pure (prop a))
    x <- runCommanderT (run (argProg (== "hello"))) ["hello"]
    x `shouldBe` Just True

  it "opt" do
    let optProg :: (Maybe String -> Bool) -> ProgramT (Opt "opt" "opt" String & Raw) IO Bool
        optProg prop = opt \o -> raw (pure (prop o))
    x <- runCommanderT (run (optProg (== Just "hello"))) ["opt", "hello"]
    x `shouldBe` Just True
    
  it "flag" do
    let flagProg :: Monad m => ProgramT (Flag "flag" & Raw) m Bool
        flagProg = flag (raw . pure)
    x <- runCommanderT (run flagProg) ["flag"]
    x `shouldBe` Just True

  describe "bigProgTests" do
    let bigProg :: Monad m => ProgramT ("argument" & Arg "arg" String & Flag "flag" & Raw + Opt "opt" "option-test" Word64 & "option" & Raw) m (Either (String,Bool) (Maybe Word64))
        bigProg = (sub @"argument" $ arg $ \a -> flag $ \f -> raw $ pure $ Left (a,f)) <+>
                  (opt \o -> sub @"option" $ raw $ pure $ Right o)
    let test prog result state = it (show state) $ runCommanderT (run prog) state >>= (`shouldBe` result)
    test bigProg (Just $ Left ("arg",True)) ["argument", "arg", "flag"]
    test bigProg (Just $ Right $ Just 0)    ["option", "opt", "0"]
    test bigProg Nothing                    ["argument"]
    test bigProg Nothing                    ["argument", "opt", "option"]
    test bigProg Nothing                    ["argument", "opt", "option", "flag"]
    test bigProg (Just $ Right $ Just 0)    ["option", "opt'", "option"]
    test bigProg (Just $ Right $ Just 1)    ["option", "opt", "1", "flag"]

  describe "optDef" do
    let optDefProg :: ProgramT (Opt "o" "opt" String & Raw) IO String
        optDefProg = optDef "Default" \o -> raw $ pure o
    let test testName result state = it testName do
          x <- runCommanderT (run optDefProg) state
          x `shouldBe` Just result
    test "default option" "Default" []
    test "option provided" "hello" ["o", "hello"]

  it "env" do
    let envProg :: forall a. Unrender a => ProgramT (Env 'Required "ONOGOTTAFINDABIGNAME" a & Raw) IO a
        envProg = env \e -> raw $ pure e
    let test :: forall a. (Unrender a, Show a, Eq a) => Maybe a -> IO ()
        test result = runCommanderT (run envProg) [] >>= (`shouldBe` result)
    test @String Nothing
    setEnv "ONOGOTTAFINDABIGNAME" "POOP"
    test @String $ Just "POOP"
    setEnv "ONOGOTTAFINDABIGNAME" "6"
    test @Int $ Just 6

  it "envOpt" do
    let envOptProg :: forall a. ProgramT (Env 'Optional "BIGNAME" a & Raw) IO (Maybe a)
        envOptProg = envOpt \e -> raw $ pure e
    let test :: forall a. (Unrender a, Show a, Eq a) => Maybe (Maybe a) -> IO ()
        test result  = runCommanderT (run $ envOptProg @a) [] >>= (`shouldBe` result)
    test @String $ Just Nothing
    setEnv "BIGNAME" "POP"
    test @String $ Just $ Just "POP"
    setEnv "BIGNAME" "3"
    test @Int $ Just $ Just 3

  it "envOptDef" do
    let envOptDefProg :: a -> ProgramT (Env 'Optional "CORPUS" a & Raw) IO a
        envOptDefProg x = envOptDef x \e -> raw $ pure e
    let test :: (Unrender a, Show a, Eq a) => a -> Maybe a -> IO ()
        test defaultVal result = runCommanderT (run $ envOptDefProg defaultVal) [] >>= (`shouldBe` result)
    test @Int 1 $ Just 1
    setEnv "CORPUS" "2"
    test @Int 1 $ Just 2
    setEnv "CORPUS" "POOP"
    test @String "POOP" $ Just "POOP"

  describe "eitherSwitch" do
    let eitherSwitchProg :: ProgramT (Arg "xy" (Either Int String) & Raw) IO (Either Int String)
        eitherSwitchProg = arg \case
          Left x -> raw $ pure $ Left x
          Right y -> raw $ pure $ Right y
    let test testName result state = it testName do
          x <- runCommanderT (run eitherSwitchProg) state
          x `shouldBe` result
    test "left" (Just $ Left 10) ["10"]
    test "right" (Just $ Right "Hello") ["Hello"]
    test "no arguments" Nothing []

