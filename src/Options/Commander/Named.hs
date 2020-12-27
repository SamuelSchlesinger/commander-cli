module Options.Commander.Named where

import GHC.TypeLits (Symbol, KnownSymbol)
import Options.Commander.Internal
import Options.Commander.Program (HasProgram(ProgramT,run,hoist,documentation))
import Options.Commander.Sequence
import Data.Tree (Tree(Node))


-- | The type level combinator for constructing 'named' programs, giving your
-- program a name at the toplevel for the sake of documentation.
data Named :: Symbol -> *

instance (KnownSymbol name, HasProgram p) => HasProgram (Named name & p) where
  newtype ProgramT (Named name & p) m a = NamedProgramT { unNamedProgramT :: ProgramT p m a }
  run = run . unNamedProgramT 
  hoist n = NamedProgramT . hoist n . unNamedProgramT
  documentation = [Node
    ("name: " <> showSymbol @name)
    (documentation @p)]

-- | Named command combinator, useful at the top level for naming
-- a program. Typically, the name will be the name or alias of the
-- executable you expect to produce.
named :: forall s p m a.
         KnownSymbol s 
      => ProgramT p m a 
      -> ProgramT (Named s & p) m a
named = NamedProgramT

