module Options.Commander.Argument where

import Options.Commander.Imports


-- | The type level 'arg'ument combinator, with a 'Symbol' designating the
-- name of that argument.
data Arg :: Symbol -> * -> *

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Arg name t & p) where
  newtype ProgramT (Arg name t & p) m a = ArgProgramT { unArgProgramT :: t -> ProgramT p m a }
  run f = Action $ \State{..} -> do
    case arguments of
      (x : xs) -> 
        case unrender x of
          Just t -> return (run (unArgProgramT f t), State{ arguments = xs, .. })  
          Nothing -> return (Defeat, State{..})
      [] -> return (Defeat, State{..})
  hoist n (ArgProgramT f) = ArgProgramT (hoist n . f)
  documentation = [Node
    ("argument: " <> showSymbol @name <> " :: " <> showTypeRep @t)
    (documentation @p)]

-- | Argument combinator
arg :: forall name x p m a.
       KnownSymbol name
    => (x -> ProgramT p m a) 
    -> ProgramT (Arg name x & p) m a 
arg = ArgProgramT

