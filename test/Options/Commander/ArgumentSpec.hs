module Options.Commander.ArgumentSpec where

import Test


spec :: Spec
spec = do
  let program :: forall a. Unrender a => ProgramT (Arg "arg" a & Raw) IO a
      program = arg \a -> raw $ pure a
  testProgram "string arg"     program (Just "hello" :: Maybe String) ["hello"]
  testProgram "int arg"        program (Just 1       :: Maybe Int)    ["1"]
  testProgram "unrender error" program (Nothing      :: Maybe Int)    ["abc"]
  testProgram "empty state"    program (Nothing      :: Maybe String) []

