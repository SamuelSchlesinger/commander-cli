{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad
import Options.Commander 
import Prelude
import System.Directory
import System.Process
import Data.List
import Text.Read
import System.Exit
import Data.Either

type TaskManager
  = Named "task-manager" &
  ( "help"       & Raw
  + "edit"       & TaskProgram
  + "open"       & TaskProgram
  + "close"      & TaskProgram
  + "tasks"      & Raw
  + "priorities" & Raw
  + Raw
  )

type TaskProgram = Arg "task-name" String & Raw
  
taskManager :: ProgramT TaskManager IO ()
taskManager = toplevel @"task-manager" 
  $   sub @"edit" editTask
  <+> sub @"open" newTask
  <+> sub @"close" closeTask
  <+> sub @"tasks" listTasks
  <+> sub @"priorities" listPriorities
  <+> hoist describeTaskManager (usage @TaskManager)
  where
    describeTaskManager :: IO a -> IO a
    describeTaskManager io = do
      putStrLn "Welcome to the Task Manager! This is a tool to help you manage tasks, each with priorities."
      io

editTask = arg @"task-name" $ \taskName -> raw 
  $ withTask taskName $ \Context{home} task -> callProcess "vim" [home ++ "/tasks/" ++ taskName ++ ".task"]

newTask = arg @"task-name" $ \taskName -> raw $ do
  Context{home, tasks} <- initializeOrFetch
  if not (taskName `elem` tasks)
    then do
      let path = home ++ "/tasks/" ++ taskName ++ ".task"
      callProcess "touch" [path]
      appendFile path (renderPriorities $ [])
      callProcess "vim" [path]
    else putStrLn $ "task " ++ taskName ++ " already exists."

closeTask = arg @"task-name" $ \taskName -> raw 
  $ withTask taskName $ \Context{home, tasks} mtask ->
    case mtask of
      Just Task{priorities} ->
        if priorities == [] 
          then removeFile (home ++ "/tasks/" ++ taskName ++ ".task")
          else putStrLn $ "task " ++ taskName ++ " has remaining priorities."
      Nothing -> putStrLn "task is corrupted"

listTasks = raw $ do
  Context{tasks} <- initializeOrFetch
  mapM_ putStrLn tasks

listPriorities = raw $ do
  Context{tasks} <- initializeOrFetch
  forM_ tasks $ \taskName -> withTask taskName $ \_ mtask -> 
    case mtask of
      Just Task{name, priorities} -> do
        putStrLn $ name ++ ": "
        putStrLn $ renderPriorities priorities
      Nothing -> putStrLn $ "Corruption! Task " ++ taskName ++ " is the culprint"

data Context = Context
  { home :: FilePath
  , tasks :: [String] }
  deriving Show

data Task = Task
  { name :: String
  , priorities :: [(String, String)]
  } deriving (Show, Read)

readTask :: String -> IO (Maybe Task)
readTask taskName = do
  Context{home, tasks} <- initializeOrFetch
  if taskName `elem` tasks
    then do
      let path = home ++ "/tasks/" ++ taskName ++ ".task"
      stringTask <- readFile path
      return $ parseTask taskName stringTask
    else do
      putStrLn $ "task " ++ taskName ++ " does not exist."
      exitSuccess

writeTask :: Task -> IO ()
writeTask Task{name, priorities} = do
  Context{home, tasks} <- initializeOrFetch
  let path = home ++ "/tasks/" ++ name ++ ".task"
  writeFile path $ renderPriorities priorities

renderPriorities :: [(String, String)] -> String
renderPriorities = concat . map (\(x, y) -> ("#" ++ x ++ "\n" ++ unlines (map ("  " ++) $ lines y)))

trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

parseTask :: String -> String -> Maybe Task
parseTask taskName
  = sectionByPriority
      . dropWhile isRight
      . map (\case
      ('#': (trimWhitespace -> xs)) -> Left xs
      xs -> Right xs)
      . lines
    where
      sectionByPriority :: [Either String String] -> Maybe Task
      sectionByPriority = \case
        Left priority : xs -> do
          let (concat . map (fromRight undefined) -> description, xs') = span isRight xs
          Task{priorities} <- sectionByPriority xs'
          Just $ Task taskName ((priority, description) : priorities)
        Right _ : xs -> Nothing
        [] -> Just $ Task taskName []

withTask :: String -> (Context -> Maybe Task -> IO ()) -> IO ()
withTask taskName action = do
  c@Context{tasks, home} <- initializeOrFetch
  if taskName `elem` tasks 
    then do
      readTask taskName >>= \case
        Just task -> action c (Just task)
        Nothing -> action c Nothing
    else putStrLn $ "task " ++ taskName ++ " does not exist."

initializeOrFetch = do
  home <- getHomeDirectory >>= makeAbsolute
  withCurrentDirectory home $ do
    doesDirectoryExist "tasks" >>= \case
      True -> Context home . map (takeWhile (/= '.')) . filter (".task" `isSuffixOf`) <$> listDirectory "tasks"
      False -> Context home [] <$ createDirectory "tasks"
  
main = command_ taskManager
