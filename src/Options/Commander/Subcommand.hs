module Options.Commander.Subcommand where

import Options.Commander.Imports


data Sub :: [Symbol] -> *

instance (SymbolList subs, HasProgram p) => HasProgram (Sub subs & p) where
  newtype ProgramT (Sub subs & p) m a = SubProgramT { unSubProgramT :: ProgramT p m a }
  run s = Action $ return . \case
    x:xs | elem x $ symbolList @subs -> (run $ unSubProgramT s, xs)
    x -> (Defeat, x)
  hoist n = SubProgramT . hoist n . unSubProgramT
  documentation = [Node
    ("subprogram: " <> intercalate ", " (symbolList @subs))
    (documentation @p)]

-- | Subcommand combinator
sub :: forall s p m a.
       KnownSymbol s 
    => ProgramT p m a 
    -> ProgramT (Sub '[s] & p) m a
sub = SubProgramT

-- | Subcommand combinator
subMulti
  :: forall ss p m a.
     SymbolList ss
  => ProgramT p m a 
  -> ProgramT (Sub ss & p) m a
subMulti = SubProgramT

