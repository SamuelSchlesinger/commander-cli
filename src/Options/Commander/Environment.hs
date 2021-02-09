module Options.Commander.Environment where

import Data.Text (pack)
import Options.Commander.Imports
import System.Environment (lookupEnv)


-- | The type level 'env'ironment variable combinator, taking a name as
-- input, allowing your program to take environment variables as input
-- automatically.
data Env :: Optionality -> Symbol -> * -> *

-- | The type level tag for whether or not a variable is required or not.
data Optionality = Required | Optional

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Env 'Required name t & p) where
  newtype ProgramT (Env 'Required name t & p) m a = EnvProgramT'Required { unEnvProgramT'Required :: t -> ProgramT p m a }
  run f = Action $ \state -> do
    val <- lookupEnv $ showSymbol @name
    case val of
      Just v ->
        case unrender (pack v) of
          Just t -> return (run (unEnvProgramT'Required f t), state)  
          Nothing -> return (Defeat, state)
      Nothing -> return (Defeat, state)
  hoist n (EnvProgramT'Required f) = EnvProgramT'Required (hoist n . f)
  documentation = [Node
    ("required env: " <> showSymbol @name <> " :: " <> showTypeRep @t)
    (documentation @p)]

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Env 'Optional name t & p) where
  data ProgramT (Env 'Optional name t & p) m a = EnvProgramT'Optional
    { unEnvProgramT'Optional :: Maybe t -> ProgramT p m a
    , unEnvDefault :: Maybe t }
  run f = Action $ \state -> do
    val <- lookupEnv $ showSymbol @name
    case val of
      Just v -> do
        case unrender @t (pack v) of
          Just t -> return (run (unEnvProgramT'Optional f (Just t)), state)  
          Nothing -> return (Defeat, state)
      Nothing -> return (run (unEnvProgramT'Optional f (unEnvDefault f)), state)
  hoist n (EnvProgramT'Optional f d) = EnvProgramT'Optional (hoist n . f) d
  documentation = [Node
    ("optional env: " <> showSymbol @name <> " :: " <> showTypeRep @t)
    (documentation @p)]

-- | Required environment variable combinator
env :: forall name p x m a.
     KnownSymbol name
  => (x -> ProgramT p m a)
  -> ProgramT (Env 'Required name x & p) m a
env = EnvProgramT'Required

-- | Optional environment variable combinator
envOpt :: forall name x p m a.
     KnownSymbol name
  => (Maybe x -> ProgramT p m a)
  -> ProgramT (Env 'Optional name x & p) m a
envOpt = flip EnvProgramT'Optional Nothing

-- | Optional environment variable combinator with default
envOptDef :: forall name x p m a.
     KnownSymbol name
  => x
  -> (x -> ProgramT p m a)
  -> ProgramT (Env 'Optional name x & p) m a
envOptDef x f = EnvProgramT'Optional { unEnvDefault = Just x, unEnvProgramT'Optional = \case { Just y -> f y; Nothing -> error "Violated invariant of optEnvDef" } }

