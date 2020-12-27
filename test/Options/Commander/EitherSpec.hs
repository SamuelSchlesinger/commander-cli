module Options.Commander.EitherSpec where

import Test


spec :: Spec
spec = do
  let program :: ProgramT (Arg "xy" (Either Int String) & Raw) IO (Either Int String)
      program = arg \case
        Left  x -> raw $ pure $ Left  x
        Right y -> raw $ pure $ Right y
  testProgram "left"         program (Just $ Left 10)       ["10"]
  testProgram "right"        program (Just $ Right "Hello") ["Hello"]
  testProgram "no arguments" program Nothing                []

