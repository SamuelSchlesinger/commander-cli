# Commander CLI

[![Hackage](https://img.shields.io/hackage/v/commander-cli.svg)](https://hackage.haskell.org/package/commander-cli)
[![Build Status](https://travis-ci.org/SamuelSchlesinger/commander-cli.svg?branch=master)](https://travis-ci.org/SamuelSchlesinger/commander-cli)

This library is meant to allow Haskell programs to quickly and easily construct
command line interfaces which are easy to use, especially as a Haskell user. To
begin, I suggest viewing/playing with the task-manager application which
comes with this repository. Its documentation is displayed as:

```
usage:
name: task-manager
|
`- optional env: TASK_DIRECTORY :: [Char]
   |
   +- subprogram: help
   |  |
   |  `- description: Displays this help text.
   |
   +- subprogram: edit
   |  |
   |  +- description: Edits an already existing task. Fails if the task does not exist.
   |  |
   |  `- argument: task-name :: [Char]
   |
   +- subprogram: open
   |  |
   |  +- description: Opens a new task for editing. Fails if the task exists already.
   |  |
   |  `- argument: task-name :: [Char]
   |
   +- subprogram: close
   |  |
   |  +- description: Closes a task. Fails if there are remaining priorities within the task.
   |  |
   |  `- argument: task-name :: [Char]
   |
   +- subprogram: tasks
   |  |
   |  `- description: Lists current tasks.
   |
   `- subprogram: priorities
      |
      `- description: Lists priorities for every task.
```


The library is based around the following classes:

```haskell
class Unrender r where
  unrender :: Text -> Maybe r
```

This class is what you will use to define the parsing of a type from text and
can use any parsing library or whatever you want. Next, we have the class

```haskell
class HasProgram p where
  data ProgramT p m a
  run :: ProgramT p IO a -> CommanderT State IO a
  hoist :: (forall x. m x -> n x) -> ProgramT p m a -> ProgramT p n a
  documentation :: Forest String
```

Instances of this class will define a syntactic element, a new instance of the
data family ProgramT, as well as its semantics in terms of the CommanderT monad,
which is a backtracking monad based on a metaphor to military commanders which
retreats upon defeat.
