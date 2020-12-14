module Options.Commander.Option where

import Options.Commander.Imports


-- | The type level 'opt'ion combinator, with a 'Symbol' designating the
-- option's name and another representing the metavariables name for
-- documentation purposes.
data Opt :: [Symbol] -> Symbol -> * -> *

instance (SymbolList options, KnownSymbol name, HasProgram p, Unrender t) => HasProgram (Opt options name t & p) where
  data ProgramT (Opt options name t & p) m a = OptProgramT
    { unOptProgramT :: Maybe t -> ProgramT p m a
    , unOptDefault :: Maybe t }
  run f = Action $ return . recurseOpt
    where
    recurseOpt = \case
      d@(x:y:xs)
        | elem x $ symbolList @options -> case unrender y of
           Just t -> (, xs) $ run $ unOptProgramT f $ Just t
           Nothing -> (Defeat,d) 
      x:xs -> (x :) <$> recurseOpt xs
      [] -> (,[]) $ run $ unOptProgramT f $ unOptDefault f
  hoist n (OptProgramT f d) = OptProgramT (hoist n . f) d
  documentation = [Node
    ("option: " <> intercalate ", " (symbolList @options) <> " <" <> showSymbol @name <> " :: " <> showTypeRep @t <> ">")
    (documentation @p)]

-- | Option combinator
opt
  :: forall option name x p m a.
     (KnownSymbol option, KnownSymbol name)
  => (Maybe x -> ProgramT p m a) 
  -> ProgramT (Opt '[option] name x & p) m a
opt = optMulti

-- | Option combinator with multiple option matchs
optMulti
  :: forall options name x p m a.
   (KnownSymbol name)
  => (Maybe x -> ProgramT p m a) 
  -> ProgramT (Opt options name x & p) m a
optMulti = flip OptProgramT Nothing

-- | Option combinator with a default value
optDef
  :: forall option name x p m a.
     (KnownSymbol option, KnownSymbol name)
  => x
  -> (x -> ProgramT p m a)
  -> ProgramT (Opt '[option] name x & p) m a
optDef = optDefMulti

-- | Option combinator with a default value that has multiple option matchs
optDefMulti
  :: forall options name x p m a.
     (SymbolList options, KnownSymbol name)
  => x
  -> (x -> ProgramT p m a)
  -> ProgramT (Opt options name x & p) m a
optDefMulti x f = OptProgramT
  { unOptDefault = Just x
  , unOptProgramT = maybe (error "Violated invariant of optDef") f
  -- , unOptProgramT = \case
  --   Just x -> f x
  --   Nothing -> error "Violated invariant of optDef"
  }

