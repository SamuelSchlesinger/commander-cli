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
import System.Environment (setEnv)
import Data.Maybe
import Control.Arrow ((>>>))

main :: IO ()
main =
     rawTest
  >> argTest
  >> optTest
  >> flagTest
  >> bigProgTests
  >> optDefTest
  >> envTest
  >> envOptTest
  >> envOptDefTest
  >> eitherSwitchTest
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
optDefTest = 
     parlay (pure ()) exitFailure (== "Default") (State mempty mempty mempty)
  >> parlay (pure ()) exitFailure (== "hello") (State mempty (HashMap.fromList [("o", "hello")]) mempty)
  >> parlay exitFailure (pure ()) (== "Default0") (State mempty mempty mempty)
  >> parlay exitFailure (pure ()) (== "hello0") (State mempty (HashMap.fromList [("o", "hello")]) mempty)
  where
  parlay y n prop state = maybe exitFailure (cond y n) =<< runCommanderT (run (optDefProg prop)) state

envProg :: (x -> Bool) -> ProgramT (Env 'Required "ONOGOTTAFINDABIGNAME" x & Raw) IO Bool
envProg prop = env \e -> raw (pure (prop e))

envTest :: IO ()
envTest = do
  putStrLn "Testing environment variables"
  testBool =<< isNothing <$> test (envProg (== ("POOP" :: String))) (State mempty mempty mempty)
  setEnv "ONOGOTTAFINDABIGNAME" "POOP"
  testMaybeBool =<< test (envProg (== ("POOP" :: String))) (State mempty mempty mempty)
  setEnv "ONOGOTTAFINDABIGNAME" "POP"
  testMaybeBool =<< fmap not <$> test (envProg (== ("POOP" :: String))) (State mempty mempty mempty)

envOptProg :: (Maybe x -> Bool) -> ProgramT (Env 'Optional "BIGNAME" x & Raw) IO Bool
envOptProg prop = envOpt \e -> raw (pure (prop e))

envOptProg' :: ProgramT (Env 'Optional "BIGNAME" x & Raw) IO (Maybe x)
envOptProg' = envOpt \e -> raw (pure e)

envOptTest :: IO ()
envOptTest = do
  putStrLn "Testing optional environment variables"
  testMaybeBool =<< test (envOptProg (== (Nothing @Bool))) (State mempty mempty mempty)
  setEnv "BIGNAME" "POP"
  testMaybeBool =<< fmap not <$> test (envOptProg (== (Just ("POOP" :: String)))) (State mempty mempty mempty)
  testMaybeBool =<< test (envOptProg (== (Just ("POP" :: String)))) (State mempty mempty mempty)

envOptDefProg :: x -> (x -> Bool) -> ProgramT (Env 'Optional "CORPUS" x & Raw) IO Bool
envOptDefProg x prop = envOptDef x \e -> raw (pure (prop e))

envOptDefTest :: IO ()
envOptDefTest = do
  putStrLn "Testing optional environment variables with hard-coded defaults"
  testMaybeBool =<< test (envOptDefProg (10 :: Int) (== 10)) (State mempty mempty mempty)
  testMaybeBool =<< fmap not <$> test (envOptDefProg "POP" (== ("POOP" :: String))) (State mempty mempty mempty)
  setEnv "CORPUS" "POOP"
  testMaybeBool =<< test (envOptDefProg "POP" (== ("POOP" :: String))) (State mempty mempty mempty)

eitherSwitchProg :: (x -> Bool) -> (y -> Bool) -> ProgramT (Arg "xy" (AltEither x y) & Raw) IO (Either Bool Bool)
eitherSwitchProg xpred ypred = arg $ altEither >>> \case
  Left x -> raw $ pure $ Left $ xpred x
  Right y -> raw $ pure $ Right $ ypred y

eitherSwitchTest  :: IO ()
eitherSwitchTest = do
  let example = eitherSwitchProg (== (10 :: Int)) (== ("Hello" :: String))
  let ploof a c = runCommanderT (run example) (State a mempty mempty) >>= c
  ploof ["10"] \case
    Just (Left True) -> pure ()
    _ -> exitFailure
  ploof ["Hello"] \case
    Just (Right True) -> pure ()
    _ -> exitFailure
  ploof ["Poop"] \case
    Just (Right False) -> pure ()
    _ -> exitFailure
  ploof [] \case
    Nothing -> pure ()
    _ -> exitFailure

