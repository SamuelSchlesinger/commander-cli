module Options.Commander.Annotated where

import Options.Commander.Imports


-- | The type level 'annotated' combinator, allowing a command line 
data Annotated :: Symbol -> * -> *

instance (KnownSymbol annotation, HasProgram (combinator & p)) => HasProgram (Annotated annotation combinator & p) where
  newtype ProgramT (Annotated annotation combinator & p) m a = AnnotatedProgramT { unAnnotatedProgramT :: ProgramT (combinator & p) m a }
  run = run . unAnnotatedProgramT 
  hoist n = AnnotatedProgramT . hoist n . unAnnotatedProgramT
  documentation
    = fmap (\(Node x s) -> Node (x <> ", " <> showSymbol @annotation) s)
    . documentation
    . unAnnotatedProgramT

-- | A combinator which augments the documentation of the next element, by
-- adding a description after its name and type.
annotated :: forall annotation combinator p m a. ProgramT (combinator & p) m a -> ProgramT (Annotated annotation combinator & p) m a
annotated = AnnotatedProgramT

