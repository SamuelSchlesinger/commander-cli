module Task where

data Context = Context
  { home :: FilePath
  , tasks :: [String] }

data Task = Task
  { name :: String
  , priorities :: [(String, String)]
  }
