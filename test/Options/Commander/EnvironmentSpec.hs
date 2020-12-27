module Options.Commander.EnvironmentSpec where

import Test
import System.Environment (setEnv)
import Data.List.NonEmpty (NonEmpty((:|)))


spec :: Spec
spec = do
  it "env" do
    let program :: forall a. Unrender a => ProgramT (Env 'Required '["ONOGOTTAFINDABIGNAME"] a & Raw) IO a
        program = env \e -> raw $ pure e
    let test result = runCommanderT (run program) [] >>= (`shouldBe` result)
    test (Nothing     :: Maybe String)
    setEnv "ONOGOTTAFINDABIGNAME" "POOP"
    test (Just "POOP" :: Maybe String)
    setEnv "ONOGOTTAFINDABIGNAME" "6"
    test (Just 6      :: Maybe Int)

  it "envMulti" do
    let program :: forall a. Unrender a => ProgramT (Env 'Required '["ENVMULTI","ENVMULTI2"] a & Raw) IO a
        program = envMulti \e -> raw $ pure e
    let test result = runCommanderT (run program) [] >>= (`shouldBe` result)
    test (Nothing     :: Maybe String)
    setEnv "ENVMULTI" "POOP"
    test (Just "POOP" :: Maybe String)
    setEnv "ENVMULTI2" "6"
    test (Just 6      :: Maybe Int)

  it "envOpt" do
    let program :: forall a. ProgramT (Env ('Optional 'Nothing) '["BIGNAME"] a & Raw) IO (Maybe a)
        program = envOpt \e -> raw $ pure e
    let test result = runCommanderT (run $ program) [] >>= (`shouldBe` result)
    test $ Just $ (Nothing    :: Maybe String)
    setEnv "BIGNAME" "POP"
    test $ Just $ (Just "POP" :: Maybe String)
    setEnv "BIGNAME" "3"
    test $ Just $ (Just 3     :: Maybe Int)

  describe "envOptMulti" do
    let program :: forall a. ProgramT (Env ('Optional 'Nothing) '["EOM","EOM1"] a & Raw) IO (Maybe a)
        program = envOptMulti \e -> raw $ pure e
    let test result = runCommanderT (run $ program) [] >>= (`shouldBe` result)

    it "nothing if not found" do
      test $ Just $ (Nothing :: Maybe String)

    it "tries next env var value if unrender fails" do
      setEnv "EOM" "abc"
      setEnv "EOM1" "3"
      test $ Just $ (Just 3     :: Maybe Int)

    it "multiple env-vars resolve a value" do
      setEnv "EOM" "POP"
      setEnv "EOM1" "4"
      test $ Just $ (Just "POP" :: Maybe String)
      test $ Just $ (Just 4     :: Maybe Int)

    it "defeat if all have a parse error" do
      setEnv "EOM" "abc"
      setEnv "EOM1" "def"
      test $ (Nothing :: Maybe (Maybe Int))

  describe "envOptDefMulti" do
    it "Int" do
      let program :: ProgramT (Env ('Optional ('Just "1")) '["ENVOPTDEFMULTI","ENVOPTDEFMULTI2"] Int & Raw) IO Int
          program = $(envOptDefMulti @Int ("ENVOPTDEFMULTI" :| ["ENVOPTDEFMULTI2"]) "1") \e -> raw $ pure e
      let test result = runCommanderT (run program) [] >>= (`shouldBe` result)
      test $ Just 1
      setEnv "ENVOPTDEFMULTI2" "2"
      test $ Just 2
      setEnv "ENVOPTDEFMULTI" "3"
      test $ Just 3

    it "without type definition" do
      let program = $(envOptDefMulti @String ("ENVOPTDEFMULTIWITHOUT" :| ["ENVOPTDEFMULTIWITHOUT2"]) "ABC") \e -> raw $ pure e
      let test result = runCommanderT (run program) [] >>= (`shouldBe` result)
      test $ Just "ABC"
      setEnv "ENVOPTDEFMULTIWITHOUT2" "DEF"
      test $ Just "DEF"
      setEnv "ENVOPTDEFMULTIWITHOUT" "GHI"
      test $ Just "GHI"

