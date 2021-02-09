module Options.Commander.Argument where

import Options.Commander.Imports


-- | The type level 'arg'ument combinator, with a 'Symbol' designating the
-- name of that argument.
data Arg :: Symbol -> * -> *

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Arg name t & p) where
  newtype ProgramT (Arg name t & p) m a = ArgProgramT { unArgProgramT :: t -> ProgramT p m a }
  run f = Action $ return . \case
    (unrender -> Just x):xs -> (run $ unArgProgramT f x, xs)
    xs -> (Defeat,xs)
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

