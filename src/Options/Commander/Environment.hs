module Options.Commander.Environment where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (pack)
import Language.Haskell.TH (ExpQ, TypeQ, stringE)
import Options.Commander.Imports
import System.Environment (lookupEnv)


-- | The type level 'env'ironment variable combinator, taking a name as
-- input, allowing your program to take environment variables as input
-- automatically.
data Env :: Optionality -> [Symbol] -> * -> *

-- | The type level tag for whether or not a variable is required or not.
data Optionality = Required | Optional (Maybe Symbol)

instance (Unrender t, SymbolList names, HasProgram p) => HasProgram (Env 'Required names t & p) where
  newtype ProgramT (Env 'Required names t & p) m a = EnvProgramT'Required { unEnvProgramT'Required :: t -> ProgramT p m a }
  run f = Action $ \state -> do
    val <- altMay (fmap (>>= unrender . pack) . lookupEnv) $ symbolList @names -- this may not fit into the design where unrender failurs use alternative and not just defeated at a unrender failure
    return . (, state) $ case val of
      Just t -> run $ unEnvProgramT'Required f t
      Nothing -> Defeat
  hoist n (EnvProgramT'Required f) = EnvProgramT'Required (hoist n . f)
  documentation = [Node
    ("required env: " <> intercalate ", " (symbolList @names) <> " :: " <> showTypeRep @t)
    (documentation @p)]

instance (Unrender t, SymbolList names, HasProgram p, MaybeSymbol def) => HasProgram (Env ('Optional def) names t & p) where
  newtype ProgramT (Env ('Optional def) names t & p) m a = EnvProgramT'Optional {unEnvProgramT'Optional :: Maybe t -> ProgramT p m a}
  run f = Action $ \state ->
    traverse lookupEnv (symbolList @names) >>=
    catMaybes >>> (fmap (unrender . pack) :: [String] -> [Maybe t]) >>>
    return . (, state) . \case
    [] -> run $ unEnvProgramT'Optional f Nothing
    xs -> case catMaybes xs of
      [] -> Defeat
      x:_ -> run $ unEnvProgramT'Optional f $ Just x
  hoist n (EnvProgramT'Optional f) = EnvProgramT'Optional (hoist n . f)
  documentation = [Node
    ("optional env: " <> intercalate ", " (symbolList @names) <> " :: " <> showTypeRep @t <> defaultValDoc)
    (documentation @p)]
    where defaultValDoc = fromMaybe "" $ (" :: default of \"" <>) <$> maybeSymbol @def <&> (<> "\"")

-- | Required environment variable combinator
env :: forall name p x m a.
     KnownSymbol name
  => (x -> ProgramT p m a)
  -> ProgramT (Env 'Required '[name] x & p) m a
env = EnvProgramT'Required

-- | Required environment variable combinator with multiple names resolving to the same variable
envMulti
  :: forall names p x m a.
     SymbolList names
  => (x -> ProgramT p m a)
  -> ProgramT (Env 'Required names x & p) m a
envMulti = EnvProgramT'Required

-- | Optional environment variable combinator
envOpt :: forall name x p m a.
     KnownSymbol name
  => (Maybe x -> ProgramT p m a)
  -> ProgramT (Env ('Optional 'Nothing) '[name] x & p) m a
envOpt = envOptMulti

-- | Optional environment variable combinator with multiple names resolving to the same variable
envOptMulti
  :: forall names x p m a.
     SymbolList names
  => (Maybe x -> ProgramT p m a)
  -> ProgramT (Env ('Optional 'Nothing) names x & p) m a
envOptMulti = EnvProgramT'Optional

-- | Optional environment variable combinator with default
envOptDef :: forall t. Unrender t => String -> String -> ExpQ
envOptDef = envOptDefMulti @t . pure

-- | Optional environment variable combinator with default
envOptDefMulti :: forall t. Unrender t => NonEmpty String -> String -> ExpQ
envOptDefMulti names def = do
  checkUnrender @t def
  [e| EnvProgramT'Optional . (. fromMaybe (fromJust $ unrender $(stringE def))) :: ($(t) -> ProgramT p m a) -> ProgramT (Env ('Optional ('Just $(symbolType def))) $(ns) $(t) & p) m a |]
  where
  t :: TypeQ
  t = fromTypeable @t
  ns :: TypeQ
  ns = promotedSymbolList $ fmap symbolType names

