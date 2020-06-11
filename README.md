# commander-cli

[![Hackage](https://img.shields.io/hackage/v/commander-cli.svg)](https://hackage.haskell.org/package/commander-cli)
[![Build Status](https://travis-ci.org/SamuelSchlesinger/commander-cli.svg?branch=master)](https://travis-ci.org/SamuelSchlesinger/commander-cli)

The commander-cli package contains two DSLs for describing command line programs, 
one at the type level and one at the term level. The one at the type level looks 
like this:

```haskell
type File = "writer" & Arg "file" FilePath & Arg "contents" FilePath & Raw
          + "reader" & Arg "file" FilePath & Raw
```

This is a type which encodes information about an command line program we want to write. We can
instantiate a term of this type by writing

```haskell
file :: ProgramT File IO
file = sub (arg $ \file -> arg $ \contents -> raw $ writeFile file contents) 
   :+: sub (arg $ \file -> raw $ readFile file >>= putStrLn)
```

I can write a term of this type without specifying the File type by using the
TypeApplications extension.

```haskell
file = sub @"writer" (arg @"file" $ \file -> arg @"contents" $ \contents -> raw $ writeFile file contents)
   :+: sub @"reader" (arg @"file" $ \file -> raw $ readFile file >>= putStrLn)
```

The library consists of a few basic types which are important for understanding
how to use it. The first thing is the class

```haskell
class Unrender r where
  unrender :: Text -> Maybe r
```

This class is what you will use to define the parsing of a type from text and
can use any parsing library or whatever you want. Next, we have the class

```haskell
class HasProgram p where
  data ProgramT p m a
  run :: ProgramT p IO a -> CommanderT State IO a
  hoist :: (forall x. m x -> n x) -> ProgramT p m a -> ProgramT p n a
  invocations :: [Text]
```

Instances of this class will define a syntactic element, a new instance of the
data family ProgramT, as well as its semantics in terms of the CommanderT monad,
which is a backtracking monad based on a metaphor to military commanders which
retreats upon defeat.
