module Options.Commander.Subcommand where

import Options.Commander.Imports


data Sub :: Symbol -> *

instance (KnownSymbol sub, HasProgram p) => HasProgram (Sub sub & p) where
  newtype ProgramT (Sub sub & p) m a = SubProgramT { unSubProgramT :: ProgramT p m a }
  run s = Action $ return . \case
    x:xs | x == showSymbol @sub -> (run $ unSubProgramT s, xs)
    x -> (Defeat, x)
  hoist n = SubProgramT . hoist n . unSubProgramT
  documentation = [Node
    ("subprogram: " <> showSymbol @sub)
    (documentation @p)]

-- | Subcommand combinator
sub :: forall s p m a.
       KnownSymbol s 
    => ProgramT p m a 
    -> ProgramT (Sub s & p) m a
sub = SubProgramT

