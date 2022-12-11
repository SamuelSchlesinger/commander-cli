module Options.Commander.Option
  ( Opt
  , opt
  , optMulti
  , optDef
  , optDefMulti
  ) where

import Data.List.NonEmpty (NonEmpty)
import Options.Commander.Imports
import Language.Haskell.TH (ExpQ, TypeQ, stringE)


-- | The type level 'opt'ion combinator, with a 'Symbol' designating the
-- option's name and another representing the metavariables name for
-- documentation purposes.
data Opt :: [Symbol] -> Symbol -> Maybe Symbol -> * -> *

instance (SymbolList options, KnownSymbol name, MaybeSymbol def, HasProgram p, Unrender t) => HasProgram (Opt options name def t & p) where
  newtype ProgramT (Opt options name def t & p) m a = OptProgramT {unOptProgramT :: Maybe t -> ProgramT p m a}
  run f = Action $ return . recurseOpt
    where
    recurseOpt = \case
      d@(x:y:xs)
        | elem x $ symbolList @options -> case unrender y of
           Just t -> (,xs) $ run $ unOptProgramT f $ Just t
           Nothing -> (Defeat,d) 
      x:xs -> (x:) <$> recurseOpt xs
      [] -> (,[]) $ run $ unOptProgramT f $ unrender =<< maybeSymbol @def
  hoist n (OptProgramT f) = OptProgramT (hoist n . f)
  documentation = [Node
    ("option: " <> intercalate ", " (symbolList @options) <> " <" <> showSymbol @name <> " :: " <> showTypeRep @t <> defaultValDoc <> ">")
    (documentation @p)]
    where defaultValDoc = fromMaybe "" $ (" :: default of \"" <>) <$> maybeSymbol @def <&> (<> "\"")

-- | Option combinator
opt
  :: forall option name x p m a.
     (KnownSymbol option, KnownSymbol name)
  => (Maybe x -> ProgramT p m a) 
  -> ProgramT (Opt '[option] name 'Nothing x & p) m a
opt = optMulti

-- | Option combinator with multiple option matchs
optMulti
  :: forall options name x p m a.
   (KnownSymbol name)
  => (Maybe x -> ProgramT p m a) 
  -> ProgramT (Opt options name 'Nothing x & p) m a
optMulti = OptProgramT

-- | Option combinator with a default value
-- Result type: 
--  :: forall option name x p m a.
--     (KnownSymbol option, KnownSymbol name)
--  => (x -> ProgramT p m a) 
--  -> ProgramT (Opt '[option] name 'Nothing x & p) m a
optDef
  :: forall x.
     Unrender x
  => Option -> Name -> DefaultValue -> ExpQ
optDef option name def = optDefMulti @x (pure option) name def

-- | Option combinator with a default value that has multiple option matchs
-- Result type: 
-- :: forall options name x p m a.
--  (KnownSymbol name)
-- => (Maybe x -> ProgramT p m a) 
-- -> ProgramT (Opt options name 'Nothing x & p) m a
optDefMulti
  :: forall x.
     Unrender x
  => NonEmpty Option -> Name -> DefaultValue -> ExpQ
optDefMulti options name def = do
  checkUnrender @x def
  [e| OptProgramT . (. fromMaybe (fromJust $ unrender $(stringE def))) :: ($(x) -> ProgramT p m a) -> ProgramT (Opt $(os) $(symbolType name) ('Just $(symbolType def)) $(x) & p) m a |]
  where
  x :: TypeQ
  x = fromTypeable @x
  os :: TypeQ
  os = promotedSymbolList $ fmap symbolType options

type Option = String
type Name = String
type DefaultValue = String

