{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Task.IO where

import System.Directory
import Data.List
import Task
import Task.Codec
import System.Exit

initializeOrFetch :: FilePath -> IO Context
initializeOrFetch tasksFilePath = do
  home <- getHomeDirectory >>= makeAbsolute
  withCurrentDirectory home $ do
    doesDirectoryExist tasksFilePath >>= \case
      True -> Context home . map (takeWhile (/= '.')) . filter (".task" `isSuffixOf`) <$> listDirectory tasksFilePath
      False -> Context home [] <$ createDirectory tasksFilePath

readTask :: FilePath -> String -> IO (Maybe Task)
readTask tasksFilePath taskName = do
  Context{home, tasks} <- initializeOrFetch tasksFilePath
  if taskName `elem` tasks
    then do
      let path = home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task"
      stringTask <- readFile path
      return $ parseTask taskName stringTask
    else do
      putStrLn $ "task " ++ taskName ++ " does not exist."
      exitSuccess

writeTask :: FilePath -> Task -> IO ()
writeTask tasksFilePath Task{name, priorities} = do
  Context{home} <- initializeOrFetch tasksFilePath
  let path = home ++ "/" <> tasksFilePath <> "/" ++ name ++ ".task"
  writeFile path $ renderPriorities priorities

withTask :: FilePath -> String -> (Context -> Maybe Task -> IO ()) -> IO ()
withTask tasksFilePath taskName action = do
  c@Context{tasks} <- initializeOrFetch tasksFilePath
  if taskName `elem` tasks 
    then do
      readTask tasksFilePath taskName >>= \case
        Just task -> action c (Just task)
        Nothing -> action c Nothing
    else putStrLn $ "task " ++ taskName ++ " does not exist."
