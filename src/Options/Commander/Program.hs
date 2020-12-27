module Options.Commander.Program where

import Control.Monad (void)
import Control.Monad.Commander (CommanderT, runCommanderT)
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

type State = [Text]

-- | This is a combinator which runs a 'ProgramT' with the options,
-- arguments, and flags that I get using the 'initialState' function,
-- ignoring the output of the program.
command_ :: forall p a.
            HasProgram p 
         => ProgramT p IO a 
         -> IO ()
-- command_ prog = void $ initialState >>= runCommanderT (run prog)
command_ = void . command

-- | This is a combinator which runs a 'ProgramT' with the options,
-- arguments, and flags that I get using the 'initialState' function,
-- returning 'Just' the output of the program upon successful option and argument
-- parsing and returning 'Nothing' otherwise.
command :: forall p a.
           HasProgram p 
        => ProgramT p IO a 
        -> IO (Maybe a)
command prog = runCommanderT (run prog) . fmap pack =<< getArgs

-- | Produce a 2-dimensional textual drawing of the 'Tree' description of
-- this program.
document :: forall p. HasProgram p => String
document = drawForest (documentation @p)

