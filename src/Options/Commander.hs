{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
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
of such a command line interface is:

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

If I run this program with the argument help, it will output:

@
usage:
name: file
|
+- subprogram: help
|
+- subprogram: maybe-read
|  |
|  `- argument: filename :: [Char]
|     |
|     `- flag: ~read
|
`- subprogram: maybe-write
   |
   `- option: -file <file-to-write :: [Char]>
@

The point of this library is mainly so that you can write command line
interfaces quickly and easily, with somewhat useful help messages, and 
not have to write any boilerplate.
-}
module Options.Commander (
  -- ** Parsing Arguments and Options
  {- |
    If you want to use a Haskell type as an argument or option, you will need
    to implement the 'Unrender' class. Your type needs to be 'Typeable' for
    the sake of generating documentation.
  -}
  Unrender(unrender),
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
  arg, opt, optDef, raw, sub, named, flag, toplevel, (<+>), usage, env, envOpt, envOptDef, description, annotated,
  -- ** Run CLI Programs
  {- |
    To run a 'ProgramT' (a specification of a CLI program), you will 
    need to use 'command' or 'command_'.
  -}
  command, command_,
  {- |
    Each 'ProgramT' has a type level description, build from these type level
    combinators.
  -}
  type (&), type (+), Arg, Opt, Named, Raw, Sub, Flag, Env, Optionality(Required, Optional), Description, Annotated,
  -- ** Interpreting CLI Programs
  {- |
    The 'HasProgram' class forms the backbone of this library, defining the
    syntax for CLI programs using the 'ProgramT' data family, and defining
    the interpretation of all of the various pieces of a CLI.
  -}
  HasProgram(run, hoist, documentation),
  ProgramT(ArgProgramT, unArgProgramT,
           OptProgramT, unOptProgramT, unOptDefault,
           RawProgramT, unRawProgramT,
           SubProgramT, unSubProgramT,
           NamedProgramT, unNamedProgramT,
           FlagProgramT, unFlagProgramT,
           EnvProgramT'Optional, unEnvProgramT'Optional, unEnvDefault,
           EnvProgramT'Required, unEnvProgramT'Required,
           DescriptionProgramT,
           AnnotatedProgramT,
           (:+:)
           ),
  -- ** The CommanderT Monad
  {- |
    The 'CommanderT' monad is how your CLI programs are interpreted by 'run'.
    It has the ability to backtrack and it maintains some state.
  -}
  CommanderT(Action, Defeat, Victory), runCommanderT, initialState, State(State, arguments, options, flags),
  -- ** Middleware for CommanderT
  {- |
    If you want to modify your interpreted CLI program, in its 'CommanderT'
    form, you can use the concept of 'Middleware'. A number of these are
    provided for debugging complex CLI programs, in case they aren't doing
    what you'd expect.
  -}
  Middleware, logState, transform, withActionEffects, withDefeatEffects, withVictoryEffects
) where

import Options.Commander.Annotated
import Options.Commander.Combine
import Options.Commander.Description
import Options.Commander.Imports
import Options.Commander.Named
import Options.Commander.Raw
import Options.Commander.Subcommand
import Options.Commander.Middleware
import Options.Commander.Flag
import Options.Commander.Argument
import Options.Commander.Environment
import Options.Commander.Option


-- | A convenience combinator that constructs the program I often want
-- to run out of a program I want to write.
toplevel :: forall s p m. (HasProgram p, KnownSymbol s, MonadIO m) 
         => ProgramT p m () 
         -> ProgramT (Named s & (Sub "help" & Raw + p)) m ()
toplevel p = named (sub (usage @(Named s & (Sub "help" & Raw + p))) <+> p)

-- | A meta-combinator that takes a type-level description of a command 
-- line program and produces a simple usage program.
usage :: forall p m. (MonadIO m, HasProgram p) => ProgramT Raw m ()
usage = raw $ do
  liftIO $ putStrLn "usage:"
  liftIO $ putStrLn (document @p)

