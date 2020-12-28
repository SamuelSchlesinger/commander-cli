module Options.Commander.Subcommand where

import Options.Commander.Imports


data Sub :: Symbol -> *

instance (KnownSymbol s, HasProgram p) => HasProgram (Sub s & p) where
  newtype ProgramT (Sub s & p) m a = SubProgramT { unSubProgramT :: ProgramT p m a }
  run s = Action $ return . \case
    x:xs | x == showSymbol @s -> (run $ unSubProgramT s, xs)
    x -> (Defeat, x)
  hoist n = SubProgramT . hoist n . unSubProgramT
  documentation = [Node
    ("subprogram: " <> showSymbol @s)
    (documentation @p)]

-- | Subcommand combinator
sub :: forall s p m a.
       KnownSymbol s 
    => ProgramT p m a 
    -> ProgramT (Sub s & p) m a
sub = SubProgramT

