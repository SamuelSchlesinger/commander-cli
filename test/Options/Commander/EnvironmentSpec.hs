module Options.Commander.EnvironmentSpec where

import Test
import System.Environment (setEnv)


spec :: Spec
spec = do
  it "env" do
    let program :: forall a. Unrender a => ProgramT (Env 'Required "ONOGOTTAFINDABIGNAME" a & Raw) IO a
        program = env \e -> raw $ pure e
    let test result = runCommanderT (run program) (takeOptions []) >>= (`shouldBe` result)
    test (Nothing     :: Maybe String)
    setEnv "ONOGOTTAFINDABIGNAME" "POOP"
    test (Just "POOP" :: Maybe String)
    setEnv "ONOGOTTAFINDABIGNAME" "6"
    test (Just 6      :: Maybe Int)

  it "envOpt" do
    let program :: forall a. ProgramT (Env 'Optional "BIGNAME" a & Raw) IO (Maybe a)
        program = envOpt \e -> raw $ pure e
    let test result = runCommanderT (run $ program) (takeOptions []) >>= (`shouldBe` result)
    test $ Just $ (Nothing    :: Maybe String)
    setEnv "BIGNAME" "POP"
    test $ Just $ (Just "POP" :: Maybe String)
    setEnv "BIGNAME" "3"
    test $ Just $ (Just 3     :: Maybe Int)

  it "envOptDef" do
    let program :: a -> ProgramT (Env 'Optional "CORPUS" a & Raw) IO a
        program x = envOptDef x \e -> raw $ pure e
    let test defaultVal result = runCommanderT (run $ program defaultVal) (takeOptions []) >>= (`shouldBe` result)
    test (1      :: Int)    $ Just 1
    setEnv "CORPUS" "2"
    test (1      :: Int)    $ Just 2
    setEnv "CORPUS" "POOP"
    test ("POOP" :: String) $ Just "POOP"

