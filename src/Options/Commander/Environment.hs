module Options.Commander.Environment where

import Options.Commander.Imports
import System.Environment (lookupEnv)
import Data.Text (pack)


-- | The type level 'env'ironment variable combinator, taking a name as
-- input, allowing your program to take environment variables as input
-- automatically.
data Env :: Optionality -> [Symbol] -> * -> *

-- | The type level tag for whether or not a variable is required or not.
data Optionality = Required | Optional

instance (Unrender t, SymbolList names, HasProgram p) => HasProgram (Env 'Required names t & p) where
  newtype ProgramT (Env 'Required names t & p) m a = EnvProgramT'Required { unEnvProgramT'Required :: t -> ProgramT p m a }
  run f = Action $ \state -> do
    val <- altMay (fmap (>>= unrender . pack) . lookupEnv) $ symbolList @names -- this may not fit into the design where unrender failurs use alternative and not just defeated at a unrender failure
    return . (, state) $ case val of
      Just t -> run $ unEnvProgramT'Required f t
      Nothing -> Defeat
  hoist n (EnvProgramT'Required f) = EnvProgramT'Required (hoist n . f)
  documentation
    = pure
    . Node ("required env: " <> intercalate ", " (symbolList @names) <> " :: " <> showTypeRep @t)
    . documentation
    . 
    . unEnvProgramT'Required

instance (Unrender t, SymbolList names, HasProgram p) => HasProgram (Env 'Optional names t & p) where
  data ProgramT (Env 'Optional names t & p) m a = EnvProgramT'Optional
    { unEnvProgramT'Optional :: Maybe t -> ProgramT p m a
    , unEnvDefault :: Maybe t }
  run f = Action $ \state ->
    traverse lookupEnv (symbolList @names) >>=
    catMaybes >>> (fmap (unrender . pack) :: [String] -> [Maybe t]) >>>
    return . (, state) . \case
    [] -> run $ unEnvProgramT'Optional f $ unEnvDefault f
    xs -> case catMaybes xs of
      [] -> Defeat
      x:_ -> run $ unEnvProgramT'Optional f $ Just x
  hoist n (EnvProgramT'Optional f d) = EnvProgramT'Optional (hoist n . f) d
  documentation
    = pure
    . Node ("optional env: " <> intercalate ", " (symbolList @names) <> " :: " <> showTypeRep @t)
    . documentation
    . unEnvDefault
  documentation = [Node
    ("optional env: " <> intercalate ", " (symbolList @names) <> " :: " <> showTypeRep @t)
    (documentation @p)]

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
  -> ProgramT (Env 'Optional '[name] x & p) m a
envOpt = envOptMulti

-- | Optional environment variable combinator with multiple names resolving to the same variable
envOptMulti
  :: forall names x p m a.
     SymbolList names
  => (Maybe x -> ProgramT p m a)
  -> ProgramT (Env 'Optional names x & p) m a
envOptMulti = flip EnvProgramT'Optional Nothing

-- | Optional environment variable combinator with default
envOptDef :: forall name x p m a.
     KnownSymbol name
  => x
  -> (x -> ProgramT p m a)
  -> ProgramT (Env 'Optional '[name] x & p) m a
envOptDef = envOptDefMulti

-- | Optional environment variable combinator with default
envOptDefMulti
  :: forall names x p m a.
     SymbolList names
  => x
  -> (x -> ProgramT p m a)
  -> ProgramT (Env 'Optional names x & p) m a
envOptDefMulti x f = EnvProgramT'Optional
  { unEnvDefault = Just x
  , unEnvProgramT'Optional = maybe (error "Violated invariant of optEnvDef") f
  }

