module Options.Commander.EnvironmentSpec where

import Test
import System.Environment (setEnv)


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
    let program :: forall a. ProgramT (Env 'Optional '["BIGNAME"] a & Raw) IO (Maybe a)
        program = envOpt \e -> raw $ pure e
    let test result = runCommanderT (run $ program) [] >>= (`shouldBe` result)
    test $ Just $ (Nothing    :: Maybe String)
    setEnv "BIGNAME" "POP"
    test $ Just $ (Just "POP" :: Maybe String)
    setEnv "BIGNAME" "3"
    test $ Just $ (Just 3     :: Maybe Int)

  describe "envOptMulti" do
    let program :: forall a. ProgramT (Env 'Optional '["EOM","EOM1"] a & Raw) IO (Maybe a)
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

  it "envOptDef" do
    let program :: a -> ProgramT (Env 'Optional '["CORPUS"] a & Raw) IO a
        program x = envOptDef x \e -> raw $ pure e
    let test defaultVal result = runCommanderT (run $ program defaultVal) [] >>= (`shouldBe` result)
    test (1      :: Int)    $ Just 1
    setEnv "CORPUS" "2"
    test (1      :: Int)    $ Just 2
    setEnv "CORPUS" "POOP"
    test ("POOP" :: String) $ Just "POOP"

  it "envOptDefMulti" do
    let program :: a -> ProgramT (Env 'Optional '["ENVOPTDEFMULTI","ENVOPTDEFMULTI2"] a & Raw) IO a
        program x = envOptDefMulti x \e -> raw $ pure e
    let test defaultVal result = runCommanderT (run $ program defaultVal) [] >>= (`shouldBe` result)
    test (1      :: Int)    $ Just 1
    setEnv "ENVOPTDEFMULTI2" "2"
    test (1      :: Int)    $ Just 2
    setEnv "ENVOPTDEFMULTI" "POOP"
    test ("POOP" :: String) $ Just "POOP"

