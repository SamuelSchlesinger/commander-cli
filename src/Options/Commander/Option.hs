module Options.Commander.Option
  ( Opt
  , opt
  , optMulti
  , optDef
  , optDefMulti
  ) where

import Data.List.NonEmpty
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
optDef = optDefMulti @t . pure

-- | Option combinator with a default value that has multiple option matchs
optDefMulti
  :: forall t.
     Unrender t
  => NonEmpty String -> String -> String -> ExpQ
optDefMulti options name def = do
  when (isNothing $ unrender @t $ fromString def) $ fail $ "Unrender faild for type \"" <> showTypeRep @t <> "\" on the default value \"" <> def <> "\"."
  os' <- os
  runIO $ putStrLn "os: " *> print os'
  [e| OptProgramT . (. fromMaybe (fromJust $ unrender $(stringE def))) :: ($(t) -> ProgramT p m a) -> ProgramT (Opt $(os) $(symbolType name) ('Just $(symbolType def)) $(t) & p) m a |]
  where
  t :: TypeQ
  t = fromTypeable @t
  os :: TypeQ
  os = promoted $ fmap symbolType options
  promoted :: Foldable f => f TypeQ -> TypeQ
  promoted = foldr (\x y -> promotedConsT `appT` x `appT` y) [t|('[] :: [Symbol])|]

