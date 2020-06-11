{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{- |
Module: Options.Commander
Description: A set of combinators for constructing and executing command line programs
Copyright: (c) Samuel Schlesinger 2020
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows

Commander is an embedded domain specific language describing a command line
interface, along with ways to run those as real programs. An complete example
of such a command line interface can be found as:

@
main :: IO ()
main = command_ . toplevel @"file" $
 (sub @"maybe-read" $
  arg @"filename" \filename ->
  flag @"read" \b -> raw $
    if b
      then putStrLn =<< readFile filename
      else pure ())
  \<+\>
 (sub @"maybe-write" $
  opt @"file" @"file-to-write" \mfilename -> raw $
    case mfilename of
      Just filename -> putStrLn =<< readFile filename
      Nothing -> pure ())
@

The point of this library is mainly so that you can write command line
interfaces quickly and easily, and not have to write any boilerplate.
-}
module Options.Commander (
  -- ** Parsing Arguments and Options
  Unrender(unrender),
  -- ** Run CLI Programs
  command, command_,
  -- ** CLI Combinators
  arg, opt, raw, sub, named, flag, toplevel, (<+>), usage,
  -- ** Type Level CLI Description
  type (&), type (+), Arg, Opt, Named, Raw, Flag,
  -- **
  HasProgram(run, hoist, invocations),
  ProgramT(ArgProgramT, unArgProgramT,
           OptProgramT, unOptProgramT,
           RawProgramT, unRawProgramT,
           SubProgramT, unSubProgramT,
           NamedProgramT, unNamedProgramT,
           FlagProgramT, unFlagProgramT,
           (:+:)
           ),
  -- ** The CommanderT Monad
  CommanderT(Action, Defeat, Victory), runCommanderT, initialState, State(State, arguments, options, flags),
  -- ** Middleware for CommanderT
  Middleware, logState
) where

import Control.Applicative (Alternative(..))
import Control.Arrow (first)
import Control.Monad ((<=<))
import Control.Monad (ap, void)
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Int
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack, stripPrefix, find)
import Data.Text.Read (decimal, signed)
import Data.Word
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GHC.Generics (Generic)
import Numeric.Natural
import System.Environment (getArgs)
import Data.Typeable (Typeable, typeRep)

-- | A class for interpreting command line arguments into Haskell types.
class Typeable t => Unrender t where
  unrender :: Text -> Maybe t

instance Unrender String where
  unrender = Just . unpack

instance Unrender Text where
  unrender = Just

-- | A useful default unrender for small, bounded data types.
unrenderSmall :: (Enum a, Bounded a, Show a) => Text -> Maybe a
unrenderSmall = flip Prelude.lookup [(pack $ show x, x) | x <- [minBound..maxBound]]

instance Unrender () where
  unrender = unrenderSmall

instance Unrender a => Unrender (Maybe a) where
  unrender x = justCase x <|> nothingCase x where
    justCase x' = do
      x'' <- stripPrefix "Just " x'
      return (unrender x'')
    nothingCase x' = if x' == "Nothing" then return Nothing else Nothing

instance (Unrender a, Unrender b) => Unrender (Either a b) where
  unrender x = leftCase x <|> rightCase x where
    leftCase  = fmap Left  . unrender <=< stripPrefix "Left "
    rightCase = fmap Right . unrender <=< stripPrefix "Right "

instance Unrender Bool where
  unrender = unrenderSmall

newtype WrappedIntegral i = WrappedIntegral i
  deriving newtype (Num, Real, Ord, Eq, Enum, Integral)

instance (Typeable i, Integral i) => Unrender (WrappedIntegral i) where
  unrender = either (const Nothing) h . signed decimal where
    h (n, "") = Just (fromInteger n)
    h _ = Nothing

deriving via WrappedIntegral Integer instance Unrender Integer
deriving via WrappedIntegral Int instance Unrender Int
deriving via WrappedIntegral Int8 instance Unrender Int8
deriving via WrappedIntegral Int16 instance Unrender Int16
deriving via WrappedIntegral Int32 instance Unrender Int32
deriving via WrappedIntegral Int64 instance Unrender Int64

newtype WrappedNatural i = WrappedNatural i
  deriving newtype (Num, Real, Ord, Eq, Enum, Integral)

instance (Typeable i, Integral i) => Unrender (WrappedNatural i) where
  unrender = either (const Nothing) h . decimal where
    h (n, "") = if n >= 0 then Just (fromInteger n) else Nothing
    h _ = Nothing 

deriving via WrappedNatural Natural instance Unrender Natural
deriving via WrappedNatural Word instance Unrender Word
deriving via WrappedNatural Word8 instance Unrender Word8
deriving via WrappedNatural Word16 instance Unrender Word16
deriving via WrappedNatural Word32 instance Unrender Word32
deriving via WrappedNatural Word64 instance Unrender Word64

instance Unrender Char where
  unrender = find (const True)

-- | The type level argument combinator, with a 'Symbol' designating the
-- name of that argument.
data Arg :: Symbol -> * -> *

-- | The type level option combinator, with a 'Symbol' designating the
-- option's name and another representing the metavariables name for
-- documentation purposes.
data Opt :: Symbol -> Symbol -> * -> *

-- | The type level naming combinator, giving your program a name for the
-- sake of documentation.
data Named :: Symbol -> *

-- | The type level program sequencing combinator, taking two program types
-- and sequencing them one after another.
data (&) :: k -> * -> *
infixr 4 &

-- | The type level raw monadic program combinator, allowing a command line
-- program to just do some computation.
data Raw :: *

-- | The type level flag combinator, taking a name as input, allowing your
-- program to take flags with the syntax @~flag@.
data Flag :: Symbol -> *

-- | The type level combining combinator, taking two program types as
-- input, and being interpreted as a program which attempts to run the
-- first command line program and, if parsing its flags, subprograms,
-- options or arguments fails, runs the second, otherwise failing.
data a + b
infixr 2 +

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

-- | This is the 'State' that the 'CommanderT' library uses for its role in
-- this library. It is not inlined, because that does nothing but obfuscate
-- the 'CommanderT' monad. It consists of 'arguments', 'options', and
-- 'flags'.
data State = State 
  { arguments :: [Text]
  , options :: HashMap Text Text
  , flags :: HashSet Text
  } deriving (Generic, Show, Eq, Ord)

-- | This is the workhorse of the library. Basically, it allows you to 
-- 'run' your 'ProgramT'
-- representation of your program as a 'CommanderT' and pump the 'State'
-- through it until you've processed all of the arguments, options, and
-- flags that you have specified must be used in your 'ProgramT'. You can
-- think of 'ProgramT' as a useful syntax for command line programs, but
-- 'CommanderT' as the semantics of that program. We also give the ability
-- to 'hoist' 'ProgramT' actions between monads if you can uniformly turn
-- computations in one into another. All of the different 'invocations'
-- are also stored to give a primitive form of automatically generated
-- documentation.
class HasProgram p where
  data ProgramT p (m :: * -> *) a
  run :: ProgramT p IO a -> CommanderT State IO a
  hoist :: (forall x. m x -> n x) -> ProgramT p m a -> ProgramT p n a
  invocations :: [Text]

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Arg name t & p) where
  newtype ProgramT (Arg name t & p) m a = ArgProgramT { unArgProgramT :: t -> ProgramT p m a }
  run f = Action $ \State{..} -> do
    case arguments of
      (x : xs) -> 
        case unrender x of
          Just t -> return (run (unArgProgramT f t), State{ arguments = xs, .. })  
          Nothing -> return (Defeat, State{..})
      [] -> return (Defeat, State{..})
  hoist n (ArgProgramT f) = ArgProgramT (hoist n . f)
  invocations =
    [(("<" <> pack (symbolVal (Proxy @name))
    <> " :: " <> pack (show (typeRep (Proxy @t)))
    <> "> ") <>)] <*> invocations @p

instance (HasProgram x, HasProgram y) => HasProgram (x + y) where
  data ProgramT (x + y) m a = ProgramT x m a :+: ProgramT y m a
  run (f :+: g) = run f <|> run g
  hoist n (f :+: g) = hoist n f :+: hoist n g
  invocations = invocations @x <> invocations @y

infixr 2 :+:

instance HasProgram Raw where
  newtype ProgramT Raw m a = RawProgramT { unRawProgramT :: m a }
  run = liftIO . unRawProgramT
  hoist n (RawProgramT m) = RawProgramT (n m)
  invocations = [mempty]


instance (KnownSymbol name, KnownSymbol option, HasProgram p, Unrender t) => HasProgram (Opt option name t & p) where
  newtype ProgramT (Opt option name t & p) m a = OptProgramT { unOptProgramT :: Maybe t -> ProgramT p m a }
  run f = Action $ \State{..} -> do
    case HashMap.lookup (pack $ symbolVal (Proxy @option)) options of
      Just opt' -> 
        case unrender opt' of
          Just t -> return (run (unOptProgramT f (Just t)), State{..})
          Nothing -> return (Defeat, State{..})
      Nothing  -> return (run (unOptProgramT f Nothing), State{..})
  hoist n (OptProgramT f) = OptProgramT (hoist n . f)
  invocations =
    [(("-" <> (pack $ symbolVal (Proxy @option)) 
    <> " <" <> (pack $ symbolVal (Proxy @name)) 
    <> " :: " <> (pack $ show (typeRep (Proxy @t)))
    <> "> ") <>)  ] <*> invocations @p

instance (KnownSymbol flag, HasProgram p) => HasProgram (Flag flag & p) where
  newtype ProgramT (Flag flag & p) m a = FlagProgramT { unFlagProgramT :: Bool -> ProgramT p m a }
  run f = Action $ \State{..} -> do
    let presence = HashSet.member (pack (symbolVal (Proxy @flag))) flags
    return (run (unFlagProgramT f presence), State{..})
  hoist n = FlagProgramT . fmap (hoist n) . unFlagProgramT
  invocations = [(("~" <> (pack $ symbolVal (Proxy @flag)) <> " ") <>)] <*> invocations @p

instance (KnownSymbol name, HasProgram p) => HasProgram (Named name & p) where
  newtype ProgramT (Named name &p) m a = NamedProgramT { unNamedProgramT :: ProgramT p m a }
  run = run . unNamedProgramT 
  hoist n = NamedProgramT . hoist n . unNamedProgramT
  invocations = [((pack (symbolVal (Proxy @name)) <> " ") <>)] <*> invocations @p

instance (KnownSymbol sub, HasProgram p) => HasProgram (sub & p) where
  newtype ProgramT (sub & p) m a = SubProgramT { unSubProgramT :: ProgramT p m a }
  run s = Action $ \State{..} -> do 
    case arguments of
      (x : xs) -> 
        if x == pack (symbolVal $ Proxy @sub) 
          then return (run $ unSubProgramT s, State{arguments = xs, ..})
          else return (Defeat, State{..})
      [] -> return (Defeat, State{..})
  hoist n = SubProgramT . hoist n . unSubProgramT
  invocations = [((pack $ symbolVal (Proxy @sub) <> " ") <> )] 
            <*> invocations @p

-- | A simple default for getting out the arguments, options, and flags
-- using 'getArgs'. We use the syntax ~flag for flags and ~opt
-- for options, with arguments using the typical ordered representation.
initialState :: IO State
initialState = do
  args <- getArgs
  let (opts, args', flags) = takeOptions args
  return $ State args' (HashMap.fromList opts) (HashSet.fromList flags) 
    where
      takeOptions :: [String] -> ([(Text, Text)], [Text], [Text])
      takeOptions = go [] [] [] where
        go opts args flags (('~':x') : z) = go opts args (pack x' : flags) z
        go opts args flags (('-':x) : y : z) = go ((pack x, pack y) : opts) args flags z
        go opts args flags (x : y) = go opts (pack x : args) flags y
        go opts args flags [] = (opts, reverse args, flags)

-- | This is a combinator which runs a 'ProgramT' with the options,
-- arguments, and flags that I get using the 'initialState' function,
-- ignoring the output of the program.
command_ :: HasProgram p 
         => ProgramT p IO a 
         -> IO ()
command_ prog = void $ initialState >>= runCommanderT (run prog)

-- | This is a combinator which runs a 'ProgramT' with the options,
-- arguments, and flags that I get using the 'initialState' function,
-- returning 'Just' the output of the program upon successful option and argument
-- parsing and returning 'Nothing' otherwise.
command :: HasProgram p 
        => ProgramT p IO a 
        -> IO (Maybe a)
command prog = initialState >>= runCommanderT (run prog)

-- | Argument combinator
arg :: KnownSymbol name
    => (x -> ProgramT p m a) 
    -> ProgramT (Arg name x & p) m a 
arg = ArgProgramT

-- | Option combinator
opt :: (KnownSymbol option, KnownSymbol name)
    => (Maybe x -> ProgramT p m a) 
    -> ProgramT (Opt option name x & p) m a
opt = OptProgramT

-- | Raw monadic combinator
raw :: m a 
    -> ProgramT Raw m a
raw = RawProgramT

-- | Subcommand combinator
sub :: KnownSymbol s 
    => ProgramT p m a 
    -> ProgramT (s & p) m a
sub = SubProgramT

-- | Named command combinator, should only really be used at the top level.
named :: KnownSymbol s 
      => ProgramT p m a 
      -> ProgramT (Named s & p) m a
named = NamedProgramT

-- | Boolean flag combinator
flag :: KnownSymbol f 
     => (Bool -> ProgramT p m a) 
     -> ProgramT (Flag f & p) m a
flag = FlagProgramT

-- | A convenience combinator that constructs the program I often want
-- to run out of a program I want to write.
toplevel :: forall s p m a. (HasProgram p, KnownSymbol s, MonadIO m) 
         => ProgramT p m () 
         -> ProgramT (Named s & ("help" & Raw + p)) m ()
toplevel p = named (sub (usage @(Named s & ("help" & Raw + p))) <+> p)

-- | The command line program which consists of trying to enter one and
-- then trying the other.
(<+>) :: forall x y m a. ProgramT x m a -> ProgramT y m a -> ProgramT (x + y) m a
(<+>) = (:+:)

infixr 2 <+>

-- | A meta-combinator that takes a type-level description of a command 
-- line program and produces a simple usage program.
usage :: forall p m a. (MonadIO m, HasProgram p) => ProgramT Raw m ()
usage = raw $ do
  liftIO $ putStrLn "usage:"
  void . traverse (liftIO . putStrLn . unpack) $ invocations @p

type Middleware m = forall a. CommanderT State m a -> CommanderT State m a

logState :: MonadIO m => Middleware m
logState commander
  = case commander of
      Action a -> do
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
