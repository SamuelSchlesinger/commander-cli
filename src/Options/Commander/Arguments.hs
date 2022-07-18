module Options.Commander.Arguments
  ( Args
  , ProgramT(ArgsProgramT)
  , args
  , argsAL
  , argsN
  , argsButN
  , module Options.Commander.Arguments.Collections
  ) where

-- import Options.Commander.Arguments.AtLeast
import Options.Commander.Arguments.Collections
import Control.Monad ((>=>))
import Control.Monad.Trans.State (StateT(runStateT), mapStateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT,runMaybeT), mapMaybeT)
import Control.Monad.Trans.Class (lift)
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


-- | Arguments combinator that consumes the rest of the arguments
args
  :: forall name f t p m a
   . (Unrender t, Monad m, FromList f)
  => (f t -> ProgramT p m a)
  -> ProgramT (Args name (f t) & p) m a
args unArgsProgramT = ArgsProgramT {..}
  where unArgsConsume = (get >>= lift . MaybeT . pure . (traverse unrender >=> fromList)) <* put mempty

-- | Arguments combinator that consumes n arguments
argsN
  :: forall name n t p m a
   . (KnownNat n, Unrender t, FromList (Exact n), Traversable (Exact n), Monad m)
  => (Exact n t -> ProgramT p m a)
  -> ProgramT (Args name (Exact n t) & p) m a
argsN unArgsProgramT = ArgsProgramT {..}
  where
  unArgsConsume = do
    s <- get
    let (xs,s') = splitAt i s
    ys <- lift $ MaybeT $ pure $ traverse unrender =<< fromList xs
    put s'
    pure ys
  i = fromInteger $ natVal $ Proxy @n

-- | Arguments combinator that consumes at least `n` of the rest of the arguments
argsAL
  :: forall name f n t p m a
   . (KnownNat n, Unrender t, FromList (AtLeast f n), Traversable (AtLeast f n), Monad m)
  => (AtLeast f n t -> ProgramT p m a)
  -> ProgramT (Args name (AtLeast f n t) & p) m a
argsAL unArgsProgramT = ArgsProgramT {..}
  where
  unArgsConsume = do
    s <- get
    let (xs,s') = splitAt i s
    ys <- lift $ MaybeT $ pure $ traverse unrender =<< fromList xs
    put s'
    pure ys
  i = fromInteger $ natVal $ Proxy @n

-- | Arguments combinator that consumes all but the last n arguments
argsButN
  :: forall name n f t p m a
   . (KnownNat n, Monad m, FromList f, Traversable f, Unrender t)
  => (f t -> ProgramT p m a)
  -> ProgramT (Args name (f t) & p) m a
argsButN unArgsProgramT = ArgsProgramT {..}
  where
  unArgsConsume = do
    s <- get
    let (xs,s') = splitAt i s
    ys <- lift $ MaybeT $ pure $ traverse unrender =<< fromList xs
    put s'
    pure ys
  i = fromInteger $ natVal $ Proxy @n

