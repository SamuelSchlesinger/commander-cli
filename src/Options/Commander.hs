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
of such a command line interface is:


The point of this library is mainly so that you can write command line
interfaces quickly and easily, with somewhat useful help messages, and 
not have to write any boilerplate.
-}
module Options.Commander (
  -- ** Defining CLI Programs
  {- |
    To construct a 'ProgramT' (a specification of a CLI program), you can
    have 'arg'uments, 'opt'ions, 'raw' actions in a monad (typically IO),
    'sub'programs, 'named' programs, 'env'ironment variables, you can combine 
    programs together using '<+>', and you can generate primitive 'usage'
    information with 'usage'. There are combinators for retrieving environment
    variables as well. We also have a convenience combinator, 'toplevel',
    which lets you add a name and a help command to your program using the 'usage' combinator.
  -}
  {- |
    Each 'ProgramT' has a type level description, build from these type level
    combinators.
  -}
    toplevel, usage
  , module  Options.Commander.Annotated
  , module  Options.Commander.Argument
  , module  Options.Commander.Combine
  , module  Options.Commander.Description
  , module  Options.Commander.Environment
  , module  Options.Commander.Flag
  , module  Options.Commander.Imports
  , module  Options.Commander.Internal
  , module  Options.Commander.Middleware
  , module  Options.Commander.Named
  , module  Options.Commander.Option
  , module  Options.Commander.Raw
  , module  Options.Commander.Sequence
  , module  Options.Commander.Subcommand

  -- ** Interpreting CLI Programs
  {- |
    The 'HasProgram' class forms the backbone of this library, defining the
    syntax for CLI programs using the 'ProgramT' data family, and defining
    the interpretation of all of the various pieces of a CLI.
  -}
  -- ** Run CLI Programs
  {- |
    To run a 'ProgramT' (a specification of a CLI program), you will 
    need to use 'command' or 'command_'.
  -}
  , module  Options.Commander.Program

  -- ** Parsing Arguments and Options
  {- |
    If you want to use a Haskell type as an argument or option, you will need
    to implement the 'Unrender' class. Your type needs to be 'Typeable' for
    the sake of generating documentation.
  -}
  , module  Options.Commander.Unrender

  -- ** The CommanderT Monad
  {- |
    The 'CommanderT' monad is how your CLI programs are interpreted by 'run'.
    It has the ability to backtrack and it maintains some state.
  -}
  , module Control.Monad.Commander
) where


import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Monad.Commander

import Options.Commander.Annotated
import Options.Commander.Argument
import Options.Commander.Combine
import Options.Commander.Description
import Options.Commander.Environment
import Options.Commander.Flag
import Options.Commander.Imports
import Options.Commander.Internal
import Options.Commander.Middleware
import Options.Commander.Named
import Options.Commander.Option
import Options.Commander.Program
import Options.Commander.Raw
import Options.Commander.Sequence
import Options.Commander.Subcommand
import Options.Commander.Unrender


-- | A convenience combinator that constructs the program I often want
-- to run out of a program I want to write.
toplevel :: forall s p m. (HasProgram p, KnownSymbol s, MonadIO m) 
         => ProgramT p m () 
         -> ProgramT (Named s & (Sub '["help","-h","--help"] & Raw + p)) m ()
toplevel p = named (subMulti (usage @(Named s & (Sub '["help","-h","--help"] & Raw + p))) <+> p)

-- | A meta-combinator that takes a type-level description of a command 
-- line program and produces a simple usage program.
usage :: forall p m. (MonadIO m, HasProgram p) => ProgramT Raw m ()
usage = raw $ liftIO do
  putStrLn "usage:"
  putStrLn (document @p)

