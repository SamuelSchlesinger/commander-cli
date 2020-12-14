module Options.Commander.Combine where

import Options.Commander.Program (HasProgram(ProgramT,run,hoist,documentation))
import Control.Applicative (Alternative((<|>)))


-- | The type level combining combinator, taking two program types as
-- input, and being interpreted as a program which attempts to run the
-- first command line program and, if parsing its flags, subprograms,
-- options or arguments fails, runs the second, otherwise failing.
data a + b
infixr 2 +

instance (HasProgram x, HasProgram y) => HasProgram (x + y) where
  data ProgramT (x + y) m a = ProgramT x m a :+: ProgramT y m a
  run (f :+: g) = run f <|> run g
  hoist n (f :+: g) = hoist n f :+: hoist n g
  documentation (x :+: y) = documentation x <> documentation y
infixr 2 :+:

-- | The command line program which consists of trying to enter one and
-- then trying the other.
(<+>) :: forall x y m a. ProgramT x m a -> ProgramT y m a -> ProgramT (x + y) m a
(<+>) = (:+:)
infixr 2 <+>

