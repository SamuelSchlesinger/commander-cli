{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Task.CLI where

import Options.Commander
import GHC.TypeLits (AppendSymbol)
import System.Directory
import Task
import Task.IO
import Task.Codec
import System.Process
import Control.Monad

type TaskManager
  = Named "task-manager"
  & Annotated "the directory in which we keep our task files" (Env ('Optional ('Just "tasks")) '["TASK_DIRECTORY"] FilePath)
    & (Sub '["help"]
      & Description "Displays this help text."
      & Raw
     + TaskProgram "edit" "Edits an already existing task. Fails if the task does not exist."
     + TaskProgram "open" "Opens a new task for editing. Fails if the task exists already."
     + TaskProgram "close" "Closes a task. Fails if there are remaining priorities within the task."
     + TasklessProgram "tasks" "Lists current tasks."
     + TasklessProgram "priorities" "Lists priorities for every task."
     + Raw
    )

type TaskProgram x desc = Sub '[x] & Description desc & Annotated ("the task we're going to " `AppendSymbol` x) (Arg "task-name" String) & Raw

type TasklessProgram x desc = Sub '[x] & Description desc & Raw
  
taskManager :: ProgramT TaskManager IO ()
taskManager = named @"task-manager" . annotated . $(envOptDef @String "TASK_DIRECTORY" "tasks") $ \tasksFilePath -> 
      sub @"help" (description $ usage @TaskManager)
  <+> sub @"edit" (description $ editTask tasksFilePath) 
  <+> sub @"open" (description $ newTask tasksFilePath)
  <+> sub @"close" (description $ closeTask tasksFilePath)
  <+> sub @"tasks" (description $ listTasks tasksFilePath)
  <+> sub @"priorities" (description $ listPriorities tasksFilePath)
  <+> usage @TaskManager

editTask :: FilePath -> ProgramT (Annotated annotation (Arg "task-name" String) & Raw) IO ()
editTask tasksFilePath = annotated $ arg @"task-name" $ \taskName -> raw 
  $ withTask tasksFilePath taskName $ \Context{home} _ -> callProcess "vim" [home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task"]

newTask :: FilePath -> ProgramT (Annotated annotation (Arg "task-name" String) & Raw) IO ()
newTask tasksFilePath = annotated $ arg @"task-name" $ \taskName -> raw $ do
  Context{home, tasks} <- initializeOrFetch tasksFilePath
  if not (taskName `elem` tasks)
    then do
      let path = home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task"
      callProcess "touch" [path]
      appendFile path (renderPriorities $ [])
      callProcess "vim" [path]
    else putStrLn $ "task " ++ taskName ++ " already exists."

closeTask :: FilePath -> ProgramT (Annotated annotation (Arg "task-name" String) & Raw) IO ()
closeTask tasksFilePath = annotated $ arg @"task-name" $ \taskName -> raw 
  $ withTask tasksFilePath taskName $ \Context{home} mtask ->
    case mtask of
      Just Task{priorities} ->
        if priorities == [] 
          then removeFile (home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task")
          else putStrLn $ "task " ++ taskName ++ " has remaining priorities."
      Nothing -> putStrLn "task is corrupted"

listTasks :: FilePath -> ProgramT Raw IO ()
listTasks tasksFilePath = raw $ do
  Context{tasks} <- initializeOrFetch tasksFilePath
  mapM_ putStrLn tasks

listPriorities :: FilePath -> ProgramT Raw IO ()
listPriorities tasksFilePath = raw $ do
  Context{tasks} <- initializeOrFetch tasksFilePath
  forM_ tasks $ \taskName -> withTask tasksFilePath taskName $ \_ mtask -> 
    case mtask of
      Just Task{name, priorities} -> do
        putStrLn $ name ++ ": "
        putStrLn $ renderPriorities priorities
      Nothing -> putStrLn $ "Corruption! Task " ++ taskName ++ " is the culprint" 
