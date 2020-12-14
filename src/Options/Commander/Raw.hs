module Options.Commander.Raw where

import Options.Commander.Program (HasProgram(ProgramT,run,hoist,documentation))
import Control.Monad.IO.Class (MonadIO(liftIO))


-- | The type level 'raw' monadic program combinator, allowing a command line
-- program to just do some computation.
data Raw :: *

instance HasProgram Raw where
  newtype ProgramT Raw m a = RawProgramT { unRawProgramT :: m a }
  run = liftIO . unRawProgramT
  hoist n (RawProgramT m) = RawProgramT (n m)
  documentation = const []

-- | Raw monadic combinator
raw :: forall m a.
       m a 
    -> ProgramT Raw m a
raw = RawProgramT

