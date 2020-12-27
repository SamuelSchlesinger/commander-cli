module Options.Commander.SubcommandSpec where

import Test


spec :: Spec
spec = do
  let program ::  ProgramT (Sub '["a"] & Raw + Sub '["b"] & Raw) IO (Either Int String)
      program = (sub $ raw $ pure $ Left 1) <+> (sub $ raw $ pure $ Right "abc")
  testProgram "first  sub command" program (Just $ Left 1)      ["a"]
  testProgram "second sub command" program (Just $ Right "abc") ["b"]
  testProgram "wrong  sub command" program Nothing              ["c"]
  testProgram "no     sub command" program Nothing              []

