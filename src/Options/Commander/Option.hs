module Options.Commander.Option where

import Data.HashMap.Strict as HashMap
import Options.Commander.Imports


-- | The type level 'opt'ion combinator, with a 'Symbol' designating the
-- option's name and another representing the metavariables name for
-- documentation purposes.
data Opt :: Symbol -> Symbol -> * -> *

instance (KnownSymbol name, KnownSymbol option, HasProgram p, Unrender t) => HasProgram (Opt option name t & p) where
  data ProgramT (Opt option name t & p) m a = OptProgramT
    { unOptProgramT :: Maybe t -> ProgramT p m a
    , unOptDefault :: Maybe t }
  run f = Action $ \State{..} -> do
    case HashMap.lookup (showSymbol @option) options of
      Just opt' -> 
        case unrender opt' of
          Just t -> return (run (unOptProgramT f (Just t)), State{..})
          Nothing -> return (Defeat, State{..})
      Nothing  -> return (run (unOptProgramT f (unOptDefault f)), State{..})
  hoist n (OptProgramT f d) = OptProgramT (hoist n . f) d
  documentation = [Node
    ("option: -" <> showSymbol @option <> " <" <> showSymbol @name <> " :: " <> showTypeRep @t <> ">")
    (documentation @p)]

-- | Option combinator
opt :: forall option name x p m a.
       (KnownSymbol option, KnownSymbol name)
    => (Maybe x -> ProgramT p m a) 
    -> ProgramT (Opt option name x & p) m a
opt = flip OptProgramT Nothing

-- | Option combinator with default
optDef :: forall option name x p m a.
     (KnownSymbol option, KnownSymbol name)
  => x
  -> (x -> ProgramT p m a)
  -> ProgramT (Opt option name x & p) m a
optDef x f = OptProgramT { unOptDefault = Just x, unOptProgramT = \case { Just y -> f y; Nothing -> error "Violated invariant of optDef" } }

