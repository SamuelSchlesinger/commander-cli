module Options.Commander.Arguments
  ( Args
  , ProgramT(ArgsProgramT)
  , args
  , module Options.Commander.Arguments.AtLeast
  ) where

import Options.Commander.Arguments.AtLeast
import Control.Monad ((>=>))
import Control.Monad.State (StateT(runStateT), lift, mapStateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT,runMaybeT), mapMaybeT)
import Data.Bifunctor (first)
import Options.Commander.Imports


-- | The type level 'arg'ument combinator, with a 'Symbol' designating the
-- name of that argument.
data Args :: Symbol -> * -> *

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Args name (f t) & p) where
  data ProgramT (Args name (f t) & p) m a = ArgsProgramT
    { unArgsProgramT :: f t -> ProgramT p m a
    , unArgsConsume :: StateT State (MaybeT m) (f t)
    }
  run f = Action $ unLift $ unArgsConsume f
    where
    unLift x s = 
      fmap (maybe (Defeat,s) (first $ run . unArgsProgramT f)) $
      runMaybeT $
      runStateT x s
  hoist n (ArgsProgramT {..}) = ArgsProgramT
    { unArgsProgramT = hoist n . unArgsProgramT
    , unArgsConsume = mapStateT (mapMaybeT n) unArgsConsume
    , ..
    }
  documentation = [Node
    ("arguments: " <> showSymbol @name <> "... :: " <> showTypeRep @t)
    (documentation @p)]


-- | Arguments combinator that consumes the rest of the arguments with at least n values
args
  :: forall name f t p m a
   . (Unrender t, Monad m, FromList f)
  => (f t -> ProgramT p m a)
  -> ProgramT (Args name (f t) & p) m a
args unArgsProgramT = ArgsProgramT
  { unArgsConsume = consumeAll
  , ..
  }

consumeAll :: forall f t m. (Unrender t, Monad m, FromList f) => StateT State (MaybeT m) (f t)
consumeAll = (get >>= lift . MaybeT . pure . (traverse unrender >=> fromList)) <* put mempty

-- -- | Arguments combinator that consumes all but the last n arguments
-- argsAllBut
--   :: 
-- 
-- args1AllBut
--   ::
-- 
-- consumeAllBut :: forall t m. (Unrender t, Monad m) => StateT State (MaybeT m) [t]

