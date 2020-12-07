module Main where

import Data.Char (toUpper)
import Control.Monad
import Options.Commander 
import Prelude
import System.Directory
import System.Process hiding (env)
import Data.List
import Text.Read
import System.Exit
import Data.Either
import GHC.TypeLits (AppendSymbol)

type TaskManager
  = Named "task-manager"
  & Annotated "the directory in which we keep our task files" (Env 'Optional "TASK_DIRECTORY" FilePath)
    & ("help"
      & Description "Displays this help text."
      & Raw
     + TaskProgram "edit" "Edits an already existing task. Fails if the task does not exist."
     + TaskProgram "open" "Opens a new task for editing. Fails if the task exists already."
     + TaskProgram "close" "Closes a task. Fails if there are remaining priorities within the task."
     + TasklessProgram "tasks" "Lists current tasks."
     + TasklessProgram "priorities" "Lists priorities for every task."
     + Raw
    )

type TaskProgram x desc = x & Description desc & Annotated ("the task we're going to " `AppendSymbol` x) (Arg "task-name" String) & Raw

type TasklessProgram x desc = x & Description desc & Raw
  
taskManager :: ProgramT TaskManager IO ()
taskManager = named @"task-manager" . annotated . envOptDef @"TASK_DIRECTORY" "tasks" $ \tasksFilePath -> 
      sub @"help" (description $ usage @TaskManager)
  <+> sub @"edit" (description $ editTask tasksFilePath) 
  <+> sub @"open" (description $ newTask tasksFilePath)
  <+> sub @"close" (description $ closeTask tasksFilePath)
  <+> sub @"tasks" (description $ listTasks tasksFilePath)
  <+> sub @"priorities" (description $ listPriorities tasksFilePath)
  <+> usage @TaskManager

editTask tasksFilePath = annotated $ arg @"task-name" $ \taskName -> raw 
  $ withTask tasksFilePath taskName $ \Context{home} task -> callProcess "vim" [home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task"]

newTask tasksFilePath = annotated $ arg @"task-name" $ \taskName -> raw $ do
  Context{home, tasks} <- initializeOrFetch tasksFilePath
  if not (taskName `elem` tasks)
    then do
      let path = home ++ "/" <> tasksFilePath <> "/" ++ taskName ++ ".task"
      callProcess "touch" [path]
      appendFile path (renderPriorities $ [])
      callProcess "vim" [path]
    else putStrLn $ "task " ++ taskName ++ " already exists."

closeTask tasksFilePath = annotated $ arg @"task-name" $ \taskName -> raw 
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

data Context = Context
  { home :: FilePath
  , tasks :: [String] }
  deriving Show

data Task = Task
  { name :: String
  , priorities :: [(String, String)]
  } deriving (Show, Read)

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
  Context{home, tasks} <- initializeOrFetch tasksFilePath
  let path = home ++ "/" <> tasksFilePath <> "/" ++ name ++ ".task"
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

withTask :: FilePath -> String -> (Context -> Maybe Task -> IO ()) -> IO ()
withTask tasksFilePath taskName action = do
  c@Context{tasks, home} <- initializeOrFetch tasksFilePath
  if taskName `elem` tasks 
    then do
      readTask tasksFilePath taskName >>= \case
        Just task -> action c (Just task)
        Nothing -> action c Nothing
    else putStrLn $ "task " ++ taskName ++ " does not exist."

initializeOrFetch :: FilePath -> IO Context
initializeOrFetch tasksFilePath = do
  home <- getHomeDirectory >>= makeAbsolute
  withCurrentDirectory home $ do
    doesDirectoryExist tasksFilePath >>= \case
      True -> Context home . map (takeWhile (/= '.')) . filter (".task" `isSuffixOf`) <$> listDirectory tasksFilePath
      False -> Context home [] <$ createDirectory tasksFilePath
  
main :: IO ()
main = command_ taskManager
