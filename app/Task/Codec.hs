{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Task.Codec where

import Data.Either
import Task

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
        Right _ : _ -> Nothing
        [] -> Just $ Task taskName []
