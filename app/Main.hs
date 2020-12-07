module Main where

import Options.Commander
import Task.CLI
 
main :: IO ()
main = command_ taskManager
