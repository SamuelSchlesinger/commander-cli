{-# LANGUAGE RankNTypes #-}
module Options.Commander.Program where

import GHC.Generics (Generic)
import Control.Monad (void)
import Control.Monad.Commander (CommanderT, runCommanderT)
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Text (Text, pack)
import Data.Tree (Forest, drawForest)
import System.Environment (getArgs)


-- | This is the workhorse of the library. Basically, it allows you to 
-- 'run' your 'ProgramT'
-- representation of your program as a 'CommanderT' and pump the 'State'
-- through it until you've processed all of the arguments, options, and
-- flags that you have specified must be used in your 'ProgramT'. You can
-- think of 'ProgramT' as a useful syntax for command line programs, but
-- 'CommanderT' as the semantics of that program. We also give the ability
-- to 'hoist' 'ProgramT' actions between monads if you can uniformly turn
-- computations in one into another. We also store 'documentation' in the
-- form of a @'Forest' 'String'@, in order to automatically generate
-- 'usage' programs.
class HasProgram p where
  data ProgramT p (m :: * -> *) a
  run :: ProgramT p IO a -> CommanderT State IO a
  hoist :: (forall x. m x -> n x) -> ProgramT p m a -> ProgramT p n a
  documentation :: Forest String

-- | This is the 'State' that the 'CommanderT' library uses for its role in
-- this library. It is not inlined, because that does nothing but obfuscate
-- the 'CommanderT' monad. It consists of 'arguments', 'options', and
-- 'flags'.
data State = State 
  { arguments :: [Text]
  , options :: HashMap Text Text
  , flags :: HashSet Text
  } deriving (Generic, Show, Eq, Ord)

-- | This is a combinator which runs a 'ProgramT' with the options,
-- arguments, and flags that I get using the 'initialState' function,
-- ignoring the output of the program.
command_ :: forall p a.
            HasProgram p 
         => ProgramT p IO a 
         -> IO ()
command_ prog = void $ initialState >>= runCommanderT (run prog)

-- | This is a combinator which runs a 'ProgramT' with the options,
-- arguments, and flags that I get using the 'initialState' function,
-- returning 'Just' the output of the program upon successful option and argument
-- parsing and returning 'Nothing' otherwise.
command :: forall p a.
           HasProgram p 
        => ProgramT p IO a 
        -> IO (Maybe a)
command prog = initialState >>= runCommanderT (run prog)

-- | Produce a 2-dimensional textual drawing of the 'Tree' description of
-- this program.
document :: forall p. HasProgram p => String
document = drawForest (documentation @p)

-- | A simple default for getting out the arguments, options, and flags
-- using 'getArgs'. We use the syntax ~flag for flags and -opt
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

