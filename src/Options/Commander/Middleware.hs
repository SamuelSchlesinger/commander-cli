module Options.Commander.Middleware where

import Control.Monad.Commander (CommanderT(Action,Defeat,Victory))
-- import Control.Monad.Trans (MonadIO(..))
import Control.Monad.IO.Class (MonadIO(..))
import Options.Commander.Program (State)
import Data.Bifunctor (first)
import Data.Functor (($>))


-- ** Middleware for CommanderT
{- |
  If you want to modify your interpreted CLI program, in its 'CommanderT'
  form, you can use the concept of 'Middleware'. A number of these are
  provided for debugging complex CLI programs, in case they aren't doing
  what you'd expect.
-}

-- | The type of middleware, which can transform interpreted command line programs
-- by meddling with arguments, options, or flags, or by adding effects for
-- every step. You can also change the underlying monad.
type Middleware m n = forall a. CommanderT State m a -> CommanderT State n a

-- | Middleware to transform the base monad with a natural transformation.
transform :: (Monad m, Monad n) => (forall a. m a -> n a) -> Middleware m n
transform f commander = case commander of
  Action a -> Action $ \state -> do
    (commander', state') <- f (a state)
    pure (transform f commander', state')
  Defeat -> Defeat
  Victory a -> Victory a 

-- | Middleware to add monadic effects for every 'Action'. Useful for
-- debugging complex command line programs.
withActionEffects :: Monad m => m a -> Middleware m m
withActionEffects ma = transform (ma *>)

-- | Middleware to have effects whenever the program might backtrack.
withDefeatEffects :: Monad m => m a -> Middleware m m
withDefeatEffects ma commander = case commander of
  Action a -> Action $ \state -> do
    (commander', state') <- a state
    pure (withDefeatEffects ma commander', state')
  Defeat -> Action $ \state -> ma $> (Defeat, state)
  Victory a -> Victory a

-- | Middleware to have effects whenever the program successfully computes
-- a result.
withVictoryEffects :: Monad m => m a -> Middleware m m
withVictoryEffects ma commander = case commander of
  Action a -> Action $ \state -> do
    (commander', state') <- a state
    pure (withVictoryEffects ma commander', state')
  Defeat -> Defeat
  Victory a -> Action $ \state -> ma $> (Victory a, state)

-- | Middleware to log the state to standard out for every step of the
-- 'CommanderT' computation.
logState :: MonadIO m => Middleware m m
logState commander
  = case commander of
      Action a ->
        Action $ \state -> do
          liftIO $ print state
          fmap (first logState) (a state)
      Defeat ->
        Action $ \state -> do
          liftIO $ print state
          pure (Defeat, state)
      Victory a ->
        Action $ \state -> do
          liftIO $ print state
          pure (Victory a, state)

