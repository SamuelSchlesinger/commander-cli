module Options.Commander.Option where

import Options.Commander.Imports


-- | The type level 'opt'ion combinator, with a 'Symbol' designating the
-- option's name and another representing the metavariables name for
-- documentation purposes.
data Opt :: Symbol -> Symbol -> * -> *

instance (KnownSymbol option, KnownSymbol name, HasProgram p, Unrender t) => HasProgram (Opt option name t & p) where
  data ProgramT (Opt option name t & p) m a = OptProgramT
    { unOptProgramT :: Maybe t -> ProgramT p m a
    , unOptDefault :: Maybe t }
  run f = Action $ return . recurseOpt
    where
    recurseOpt = \case
      d@(x:y:xs)
        | x == showSymbol @option -> case unrender y of
           Just t -> (,xs) $ run $ unOptProgramT f $ Just t
           Nothing -> (Defeat,d) 
      x:xs -> (x:) <$> recurseOpt xs
      [] -> (,[]) $ run $ unOptProgramT f $ unOptDefault f
  hoist n (OptProgramT f d) = OptProgramT (hoist n . f) d
  documentation = [Node
    ("option: " <> showSymbol @option <> " <" <> showSymbol @name <> " :: " <> showTypeRep @t <> ">")
    (documentation @p)]

-- | Option combinator
opt
  :: forall option name x p m a.
     (KnownSymbol option, KnownSymbol name)
  => (Maybe x -> ProgramT p m a) 
  -> ProgramT (Opt option name x & p) m a
opt = flip OptProgramT Nothing

-- | Option combinator with a default value
optDef
  :: forall option name x p m a.
     (KnownSymbol option, KnownSymbol name)
  => x
  -> (x -> ProgramT p m a)
  -> ProgramT (Opt option name x & p) m a
optDef x f = OptProgramT
  { unOptDefault = Just x
  , unOptProgramT = maybe (error "Violated invariant of optDef") f
  }

