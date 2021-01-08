module Options.Commander.Flag where

import Data.HashSet as HashSet
import Options.Commander.Imports


-- | The type level 'flag' combinator, taking a name as input, allowing your
-- program to take flags with the syntax @~flag@.
data Flag :: Symbol -> *

instance (KnownSymbol flag, HasProgram p) => HasProgram (Flag flag & p) where
  newtype ProgramT (Flag flag & p) m a = FlagProgramT { unFlagProgramT :: Bool -> ProgramT p m a }
  run f = Action $ \State{..} -> do
    let presence = HashSet.member (showSymbol @flag) flags
    return (run (unFlagProgramT f presence), State{..})
  hoist n = FlagProgramT . fmap (hoist n) . unFlagProgramT
  documentation = [Node
    ("flag: ~" <> showSymbol @flag)
    (documentation @p)]

-- | Boolean flag combinator
flag :: forall f p m a.
        KnownSymbol f 
     => (Bool -> ProgramT p m a) 
     -> ProgramT (Flag f & p) m a
flag = FlagProgramT

