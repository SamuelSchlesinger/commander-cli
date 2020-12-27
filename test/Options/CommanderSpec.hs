module Options.CommanderSpec where

import Test


spec :: Spec
spec = do
  describe "bigProgTests" do
    let program
          :: ProgramT
              ( Sub "argument" & Arg "arg" String & Flag '["flag"] & Raw
              + Opt '["opt"] "option-test" Word & Sub "option" & Raw )
              IO
              (Either (String,Bool) (Maybe Word))
        program = (sub @"argument" $ arg $ \a -> flag $ \f -> raw $ pure $ Left (a,f)) <+>
                  (opt \o -> sub @"option" $ raw $ pure $ Right o)
    let test result state = it (show state) $ runCommanderT (run program) state >>= (`shouldBe` result)
    test (Just $ Left ("arg",True))  ["argument","arg","flag"]
    test (Just $ Right $ Just 0)     ["option","opt","0"]
    test Nothing                     ["argument"]
    test (Just $ Left ("opt",False)) ["argument","opt","option"]
    test (Just $ Left ("opt",True))  ["argument","opt","option","flag"]
    test Nothing                     ["option","opt","option"]
    test (Just $ Right $ Just 1)     ["option","opt","1","flag"]

