{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Word (Word64)
import Data.Text
import Control.Concurrent
import Options.Commander
import Control.Monad
import System.Exit
import Control.Exception
import Data.Maybe

main :: IO ()
main = rawTest >> argTest >> optTest >> flagTest >> bigProgTests

rawProg :: ProgramT Raw IO Bool
rawProg = raw (pure True)

testMaybeBool :: Maybe Bool -> IO ()
testMaybeBool = maybe exitFailure (cond (pure ()) exitFailure)

testBool :: Bool -> IO ()
testBool = cond (pure ()) exitFailure

rawTest :: IO ()
rawTest = maybe exitFailure (cond (pure ()) exitFailure) =<< runCommanderT (run rawProg) (State mempty mempty mempty)

argProg :: (String -> Bool) -> ProgramT (Arg "arg" String & Raw) IO Bool
argProg prop = arg \a -> raw (pure (prop a))

cond :: x -> x -> Bool -> x
cond x y True = x
cond x y False = y

argTest :: IO ()
argTest = maybe exitFailure (cond (pure ()) exitFailure) =<< runCommanderT (run (argProg (== "hello"))) (State ["hello"] mempty mempty)

optProg :: (Maybe String -> Bool) -> ProgramT (Opt "opt" "opt" String & Raw) IO Bool
optProg prop = opt \o -> raw (pure (prop o))

optTest :: IO ()
optTest = maybe exitFailure (cond (pure ()) exitFailure) =<< runCommanderT (run (optProg (== Just "hello"))) (State mempty (HashMap.fromList [("opt", "hello")]) mempty)

flagProg :: Monad m => ProgramT (Flag "flag" & Raw) m Bool
flagProg = flag (raw . pure)

flagTest :: IO ()
flagTest = maybe exitFailure (cond (pure ()) exitFailure) =<< runCommanderT (run flagProg) (State mempty mempty (HashSet.fromList ["flag"]))

test :: HasProgram p => ProgramT p IO Bool -> State -> IO (Maybe Bool)
test prog state = runCommanderT (logState $ run prog) state

bigProg :: Monad m => ProgramT ("argument" & Arg "arg" String & Flag "flag" & Raw + Opt "opt" "option-test" Word64 & "option" & Raw) m Bool
bigProg = (sub @"argument" $ arg $ \a -> flag $ \f -> raw $ pure $ f && a == "arg") <+> (opt \o -> sub @"option" $ raw $ pure (o == Just 0))

bigProgTests :: IO ()
bigProgTests = do
  testMaybeBool =<< test bigProg (State ["argument", "arg"] mempty (HashSet.singleton "flag"))
  testMaybeBool =<< test bigProg (State ["option"] (HashMap.fromList [("opt", "0")]) mempty)
  testBool =<< isNothing <$> test bigProg (State ["argument"] mempty mempty)
  testBool =<< isNothing <$> test bigProg (State ["argument"] (HashMap.fromList [("opt", "option")]) mempty)
  testBool =<< isNothing <$> test bigProg (State ["argument"] (HashMap.fromList [("opt", "option")]) (HashSet.singleton "flag"))
  testBool =<< (== Just False) <$> test bigProg (State ["option"] (HashMap.fromList [("opt'", "option")]) mempty)
  testBool =<< (== Just False) <$> test bigProg (State ["option"] (HashMap.fromList [("opt", "1")]) (HashSet.singleton "flag"))

optDefProg :: (String -> Bool) -> ProgramT (Opt "o" "opt" String & Raw) IO Bool
optDefProg prop = optDef "Default" \o -> raw (pure (prop o))

optDefTest :: IO ()
optDefTest = parlay (== "Default") (State mempty mempty mempty)
          >> parlay (== "hello") (State mempty (HashMap.fromList [("o", "hello")]) mempty) where
  parlay prop state = maybe exitFailure (cond (pure ()) exitFailure) =<< runCommanderT (run (optDefProg prop)) state
