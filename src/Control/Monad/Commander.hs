{-# LANGUAGE DeriveFunctor #-}
{- |
Module: Control.Monad.Commander
Description: A monad for stateful, backtracking computations
Copyright: (c) Samuel Schlesinger 2020
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Control.Monad.Commander (
  -- ** The CommanderT Monad
  {- |
    The 'CommanderT' monad is how your CLI programs are interpreted by 'run'.
    It has the ability to backtrack and it maintains some state.
  -}
  CommanderT(Action, Defeat, Victory), runCommanderT,
) where

import Control.Monad (ap)
import Control.Monad.Trans (MonadTrans, lift, liftIO, MonadIO)
import Control.Applicative (Alternative(empty, (<|>)))

-- | A 'CommanderT' action is a metaphor for a military commander. At each
-- step, we have a new 'Action' to take, or we could have experienced
-- 'Defeat', or we can see 'Victory'. While a real life commander
-- worries about moving his troops around in order to achieve a victory in
-- battle, a 'CommanderT' worries about iteratively transforming a state 
-- to find some value. We will deal with the subset of these actions where
-- every function must decrease the size of the state, as those are the
-- actions for which this is a monad.
data CommanderT state m a
  = Action (state -> m (CommanderT state m a, state))
  | Defeat
  | Victory a
  deriving Functor

-- | We can run a 'CommanderT' action on a state and see if it has
-- a successful campaign.
runCommanderT :: Monad m 
              => CommanderT state m a 
              -> state 
              -> m (Maybe a)
runCommanderT (Action action) state = do
  (action', state') <- action state
  m <- runCommanderT action' state'
  return m
runCommanderT Defeat _ = return Nothing
runCommanderT (Victory a) _ = return (Just a)

instance (Monad m) => Applicative (CommanderT state m) where
  (<*>) = ap
  pure = Victory

instance MonadTrans (CommanderT state) where
  lift ma = Action $ \state -> do
    a <- ma
    return (pure a, state)

instance MonadIO m => MonadIO (CommanderT state m) where
  liftIO ma = Action $ \state -> do
    a <- liftIO ma
    return (pure a, state)

-- Return laws:
-- Goal: return a >>= k = k a
-- Proof: return a >>= k 
--      = Victory a >>= k 
--      = k a 
--      = k a
-- Goal: m >>= return = m
-- Proof:
--   Case 1: Defeat >>= return = Defeat
--   Case 2: Victory a >>= return 
--         = Victory a
--   Case 3: Action action >>= return
--         = Action $ \state -> do
--             (action', state') <- action state
--             return (action' >>= return, state')
--
-- Case 3 serves as an inductive proof only if action' is a strictly smaller action
-- than action!
--
--  Bind laws:
--  Goal: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--  Proof: 
--    Case 1: Defeat >>= _ = Defeat
--    Case 2: Victory a >>= (\x -> k x >>= f)
--          = k a >>= f
--          = (Victory a >>= k) >>= f
--    Case 3: Action action >>= (\x -> k x >>= h)
--          = Action $ \state -> do
--              (action', state') <- action state
--              return (action' >>= (\x -> k x >>= h), state')
--          = Action $ \state -> do
--              (action', state') <- action state
--              return ((action' >>= k) >>= h, state') -- by IH
--    On the other hand,
--            (Action action >>= k) >>= h
--          = Action (\state -> do
--              (action', state') <- action state
--              return (action' >>= k, state') >>= h
--          = Action $ \state -> do
--              (action', state') <- action state
--              return ((action' >>= k) >>= h, state')
--               
--   This completes our proof for the case when these are finite.
--   Basically, we require that the stream an action produces is strictly
--   smaller than any other streams, for all state inputs. The ways that we
--   use this monad transformer satisify this constraint. If this
--   constraint is not met, many of our functions will return bottom.
--
--   We can certainly have functions that operate on these things and
--   change them safely, without violating this constraint. All of the
--   functions that we define on CommanderT programs preserve this
--   property.
--
--   An example of a violating term might be:
--
--   violator :: CommanderT state m
--   violator = Action (\state -> return (violator, state))
--
--   The principled way to include this type would be to parameterize it by
--   a natural number and have that natural number decrease over time, but
--   to enforce that in Haskell we couldn't have the monad instance
--   anyways. This is the way to go for now, despite the type violating the
--   monad laws potentially for infinite inputs. 
instance Monad m => Monad (CommanderT state m) where
  Defeat >>= _ = Defeat
  Victory a >>= f = f a
  Action action >>= f = Action $ \state -> do
    (action', state') <- action state
    return (action' >>= f, state')

instance Monad m => Alternative (CommanderT state m) where
  empty = Defeat 
  Defeat <|> a = a 
  v@(Victory _) <|> _ = v
  Action action <|> p = Action $ \state -> do
    (action', state') <- action state 
    return (action' <|> p, state')
