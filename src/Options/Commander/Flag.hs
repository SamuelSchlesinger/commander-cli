module Options.Commander.Flag where

import Options.Commander.Imports
import Data.Bifunctor (first)


-- | The type level 'flag' combinator, taking a name as input, allowing your
-- program to take flags that resolve to booleans in your program.
data Flag :: [Symbol] -> *

instance (SymbolList flags, HasProgram p) => HasProgram (Flag flags & p) where
  newtype ProgramT (Flag flags & p) m a = FlagProgramT { unFlagProgramT :: Bool -> ProgramT p m a }
  run f = Action $ return . first (run . unFlagProgramT f) . recurseFlag
    where
    recurseFlag :: State -> (Bool,State)
    recurseFlag = \case
      x:xs | elem x $ symbolList @flags -> (True,xs)
           | otherwise -> (x :) <$> recurseFlag xs
      [] -> (False,[])
  hoist n = FlagProgramT . fmap (hoist n) . unFlagProgramT
  documentation = pure . Node ("flag: " <> intercalate ", " (symbolList @flags)) . documentation . unFlagProgramT

-- | Boolean flag combinator
flag :: forall flag p m a.
        KnownSymbol flag 
     => (Bool -> ProgramT p m a) 
     -> ProgramT (Flag '[flag] & p) m a
flag = FlagProgramT

-- | Boolean flag combinator
flagMulti
  :: forall flags p m a.
     SymbolList flags 
  => (Bool -> ProgramT p m a) 
  -> ProgramT (Flag flags & p) m a
flagMulti = FlagProgramT

