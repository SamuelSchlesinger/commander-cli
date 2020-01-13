# Commander

The commander package contains two DSLs for describing command line programs, 
one at the type level and one at the term level. The one at the term level looks 
like this:

```haskell
type File = "writer" & Arg "file" FilePath & Arg "contents" FilePath Raw
          + "reader" & Arg "file" FilePath & Raw
```

This is a type which encodes information about an API we want to run. We can
instantiate a term of this type by writing

```haskell
file :: ProgramT File IO
file = sub (arg \file -> arg \contents -> raw $ writeFile file contents) 
   :+: sub (arg \file -> raw $ readFile file >>= putStrLn)
```

I can write a term of this type without specifying the File type by using the
TypeApplications extension.

```haskell
file = sub @"writer" (arg @"file" \file -> arg @"contents" \contents -> raw $ writeFile file contents)
   :+: sub @"reader" (arg @"file" \file -> raw $ readFile file >>= putStrLn)
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
  run :: ProgramT p IO a -> CommanderT [Event] State IO a
  hoist :: (forall x. m x -> n x) -> ProgramT p m a -> ProgramT p n a
  invocations :: [Text]
```

Instances of this class will define a syntactic element, a new instance of the
data family ProgramT, as well as its semantics in terms of the CommanderT monad,
which is a backtracking monad with event tracking in order to debug its behavior.

Note that this isn't really a very open data family! It is in principle, but you
would really want to modify Event or State to do something interesting and they
are in this file. This condition is only temporary! Event should be generalized 
or yanked out entirely, and State can be generalized as well. Because ProgramT
is a data family and is thus injective, we can get great type inference and do
things like infer the type of a command line program that we have written without
types at all, excepting the type applications of the names of various things.
