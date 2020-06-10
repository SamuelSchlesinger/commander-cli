{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Text
import Control.Concurrent
import Options.Commander
import Control.Monad
import System.Exit

main :: IO ()
main = rawTest >> argTest >> optTest >> flagTest

rawProg :: ProgramT Raw IO Bool
rawProg = raw (pure True)

rawTest :: IO ()
rawTest = maybe exitFailure (cond exitSuccess exitFailure) =<< runCommanderT (run rawProg) (State mempty mempty mempty)

argProg :: (String -> Bool) -> ProgramT (Arg "arg" String & Raw) IO Bool
argProg prop = arg \a -> raw (pure (prop a))

cond :: x -> x -> Bool -> x
cond x y True = x
cond x y False = y

argTest :: IO ()
argTest = maybe exitFailure (cond exitSuccess exitFailure) =<< runCommanderT (run (argProg (== "hello"))) (State ["hello"] mempty mempty)

optProg :: (Maybe String -> Bool) -> ProgramT (Opt "opt" "opt" String & Raw) IO Bool
optProg prop = opt \o -> raw (pure (prop o))

optTest :: IO ()
optTest = maybe exitFailure (cond exitSuccess exitFailure) =<< runCommanderT (run (optProg (== Just "hello"))) (State mempty (HashMap.fromList [("opt", "hello")]) mempty)

flagProg :: ProgramT (Flag "flag" & Raw) IO Bool
flagProg = flag (raw . pure)

flagTest :: IO ()
flagTest = maybe exitFailure (cond exitSuccess exitFailure) =<< runCommanderT (run flagProg) (State mempty mempty (HashSet.fromList ["flag"]))
