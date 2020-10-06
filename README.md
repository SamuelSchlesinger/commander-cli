# Commander CLI

[![Hackage](https://img.shields.io/hackage/v/commander-cli.svg)](https://hackage.haskell.org/package/commander-cli)
[![Build Status](https://travis-ci.org/SamuelSchlesinger/commander-cli.svg?branch=master)](https://travis-ci.org/SamuelSchlesinger/commander-cli)

This library is meant to allow Haskell programs to quickly and easily construct
command line interfaces which are easy to use, especially as a Haskell user. To
begin, I suggest viewing/playing with the task-manager application which
comes with this repository. Here is its type level description:

```haskell
type TaskProgram = Arg "task-name" String & Raw

type TaskManager
  = Named "task-manager"
  & Env 'Optional "TASK_DIRECTORY" FilePath
    & ("help"
      & Description "Displays this help text."
      & Raw
     + "edit"
      & Description "Edits an already existing task. Fails if the task does not exist."
      & TaskProgram
     + "open"
      & Description "Opens a new task for editing. Fails if the task exists already."
      & TaskProgram
     + "close"
      & Description "Closes a task. Fails if there are remaining priorities within the task."
      & TaskProgram
     + "tasks"
      & Description "Lists current tasks."
      & Raw
     + "priorities" 
      & Description "Lists priorities for every task."
      & Raw
     + Raw
    )
```

The usage documentation for this description is displayed as:

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

The actual program is defined as:

```haskell
taskManager :: ProgramT TaskManager IO ()
taskManager = named @"task-manager" . envOptDef @"TASK_DIRECTORY" "tasks" $ \tasksFilePath -> 
      sub @"help" (description $ usage @TaskManager)
  <+> sub @"edit" (description $ editTask tasksFilePath) 
  <+> sub @"open" (description $ newTask tasksFilePath)
  <+> sub @"close" (description $ closeTask tasksFilePath)
  <+> sub @"tasks" (description $ listTasks tasksFilePath)
  <+> sub @"priorities" (description $ listPriorities tasksFilePath)
  <+> usage @TaskManager
  where
    editTask tasksFilePath = arg @"task-name" $ \taskName -> raw 
      $ withTask tasksFilePath taskName $ \Context{home} task -> callProcess "vim" [home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task"]

    newTask tasksFilePath = arg @"task-name" $ \taskName -> raw $ do
      Context{home, tasks} <- initializeOrFetch tasksFilePath
      if not (taskName `elem` tasks)
        then do
          let path = home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task"
          callProcess "touch" [path]
          appendFile path (renderPriorities $ [])
          callProcess "vim" [path]
        else putStrLn $ "task " ++ taskName ++ " already exists."

    closeTask tasksFilePath = arg @"task-name" $ \taskName -> raw 
      $ withTask tasksFilePath taskName $ \Context{home, tasks} mtask ->
        case mtask of
          Just Task{priorities} ->
            if priorities == [] 
              then removeFile (home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task")
              else putStrLn $ "task " ++ taskName ++ " has remaining priorities."
          Nothing -> putStrLn "task is corrupted"

    listTasks tasksFilePath = raw $ do
      Context{tasks} <- initializeOrFetch tasksFilePath
      mapM_ putStrLn tasks

    listPriorities tasksFilePath = raw $ do
      Context{tasks} <- initializeOrFetch tasksFilePath
      forM_ tasks $ \taskName -> withTask tasksFilePath taskName $ \_ mtask -> 
        case mtask of
          Just Task{name, priorities} -> do
            putStrLn $ name ++ ": "
            putStrLn $ renderPriorities priorities
          Nothing -> putStrLn $ "Corruption! Task " ++ taskName ++ " is the culprint"
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
which is something like a free backtracking monad. Users should not have to make
instances of this class, as the common CLI elements are already defined as
instances. Of course, you can if you want to, and it can be profitable to do so.
