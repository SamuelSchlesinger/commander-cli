module Options.Commander.Description where

import Options.Commander.Imports


-- | The type level 'description' combinator, allowing a command line program
-- to have better documentation.
data Description :: Symbol -> *

instance (KnownSymbol description, HasProgram p) => HasProgram (Description description & p) where
  newtype ProgramT (Description description & p) m a = DescriptionProgramT { unDescriptionProgramT :: ProgramT p m a }
  run = run . unDescriptionProgramT 
  hoist n = DescriptionProgramT . hoist n . unDescriptionProgramT
  documentation = [Node
    ("description: " <> showSymbol @description)
    []] <> documentation @p

-- | A combinator which takes a program, and a type-level 'Symbol'
-- description of that program, and produces a program here the
-- documentation is annotated with the given description.
description :: forall description p m a. (HasProgram p, KnownSymbol description) => ProgramT p m a -> ProgramT (Description description & p) m a
description = DescriptionProgramT

