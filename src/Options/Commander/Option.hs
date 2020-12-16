module Options.Commander.Option where

import Control.Monad (when)
import Options.Commander.Imports
import Data.String (IsString(fromString))
import Language.Haskell.TH
-- import Language.Haskell.TH.Lib


-- | The type level 'opt'ion combinator, with a 'Symbol' designating the
-- option's name and another representing the metavariables name for
-- documentation purposes.
data Opt :: [Symbol] -> Symbol -> Maybe Symbol -> * -> *

class MaybeSymbol a where maybeSymbol :: IsString b => Maybe b
instance KnownSymbol a => MaybeSymbol ('Just a) where maybeSymbol = Just $ showSymbol @a
instance MaybeSymbol 'Nothing where maybeSymbol = Nothing

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
    where
    defaultValDoc = fromMaybe "" $ (" :: default of \"" <>) <$> maybeSymbol @def <&> (<> "\"")

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
optDef
  :: forall t.
     Unrender t
  => String -> String -> String -> ExpQ
optDef option name def = do
  when (isNothing $ unrender @t $ fromString def) $ fail $ "Unrender faild for type \"" <> showTypeRep @t <> "\" on the default value \"" <> def <> "\"."
  [e| OptProgramT . (. fromMaybe (fromJust $ unrender $(stringE def))) :: ($(t) -> ProgramT p m a) -> ProgramT (Opt '[$(toType option)] $(toType name) ('Just $(toType def)) $(t) & p) m a |]
  where
  toType :: String -> TypeQ
  toType = litT . strTyLit
  t :: TypeQ
  t = fromTypeable @t
  fromTypeable :: forall a . Typeable a => TypeQ
  fromTypeable = fromTypeRep $ typeRep @a
  fromTypeRep :: forall a. TypeRep a -> TypeQ
  fromTypeRep = splitApps >>> \(x,xs) -> foldl appT(fromTypeCon x) $ fmap (fromTypeCon . someTypeRepTyCon) xs
  fromTypeCon :: TyCon -> TypeQ
  fromTypeCon = conT . mkName . tyConName

  -- typeRepType :: forall a. Typeable a => TypeQ
  -- typeRepType = conT $ mkName $ "Show" -- showTypeRep @a
  -- t = typeRepType @t

-- | Option combinator with a default value that has multiple option matchs
optDefMulti
  :: forall options name def x p m a.
     (SymbolList options, KnownSymbol name)
  => (x -> ProgramT p m a)
  -> ProgramT (Opt options name ('Just def) x & p) m a
optDefMulti f = OptProgramT $ maybe (error "Violated invariant of optDef") f

