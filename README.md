# Commander CLI

[![Hackage](https://img.shields.io/hackage/v/commander-cli.svg)](https://hackage.haskell.org/package/commander-cli)
[![Build Status](https://travis-ci.org/SamuelSchlesinger/commander-cli.svg?branch=master)](https://travis-ci.org/SamuelSchlesinger/commander-cli)

This library is meant to allow Haskell programmers to quickly and easily construct
command line interfaces with decent documentation.

One extension I use in these examples is `-XTypeApplications`. This extension allows us to use the `@param`
syntax to apply an type-level argument explicitly to a function with a `forall x ...` in its
type. This is as opposed to implicitly applying type-level arguments, as we do when we write
`fmap (+ 1) [1, 2, 3]`, applying the type `[]` to `fmap`. It's because of type inference in Haskell
that we don't always have to apply our types explicitly, as many other languages force you to do using
a syntax typically like `fmap<[], Int> (+ 1) [1, 2, 3]`.`.

We can go to the command line and try out this example:

```
> :set -XTypeApplications
> :t fmap @[]
fmap @[] :: (a -> b) -> [a] -> [b]
> :t fmap @[] @Int
fmap @[] @Int :: (Int -> b) -> [Int] -> [b]
> :t fmap @[] @Int @Bool
fmap @[] @Int @Bool :: (Int -> Bool) -> [Int] -> [Bool]
```

The API of `commander-cli` allows for very profitable usage of type
applications, because the description of our command line program will live
at the type level. 

Another extension we will use is `-XDataKinds`, which is only for the ability
to use strings, or the kind `Symbol`, at the type level. Kinds are just the
type of types, and so `-XDataKinds` allows us to have kinds which are actually
data in their own right, like lists, strings, numbers, and custom Haskell
data types. For us, we will use strings to represent the documentation of our
program at the type level, as well as the names of options, flags, and arguments
we want to parse. This allows us to generate documentation programs simply from
the type signature of the CLI program we build.

Our first example will show a basic command line application,
complete with help messages that display reasonable messages to the user.

```haskell
main = command_
  . toplevel @"argument-taker"
  . arg @"example-argument" $ \arg ->
    raw $ do
      putStrLn arg
```

When you run this program with `argument-taker help`, you will see:

```
usage:
name: argument-taker
|
+- subprogram: help
|
`- argument: example-argument :: [Char]
```

The meaning of this documentation is that every path in the tree is a unique command.
The one we've used is the help command. If we run this program with `argument-taker hello`
we will see:

```
hello
```

Naturally, we might want to expand on the documentation of this program, as its not quite
obvious enough what it does.

```haskell
main = command_
  . toplevel @"argument-taker"
  . arg @"example-argument" $ \arg ->
    description @"Takes the argument and prints it"
  . raw $ do
      putStrLn arg
```

Printing out the documentation again with `argument-taker help`, we see:

```haskell
usage:
name: argument-taker
|
+- subprogram: help
|
`- argument: example-argument :: [Char]
   |
   `- description: Takes the argument and prints it
```

Okay, so we can expand the documentation. But what if I have an option to pass to the same program? Well, we can pass an option like so:

```haskell
main = command_
  . toplevel @"argument-taker"
  . optDef @"m" @"mode" "Print" $ \mode ->
    arg @"example-argument" $ \arg ->
    description @"Takes the argument and prints it or not, depending on the mode" 
  . raw $ do
      if mode == "Print" then putStrLn arg else pure ()
```

Now, when we run `argument-taker help` we will see:

```
usage:
name: argument-taker
|
+- subprogram: help
|
`- option: -m <mode :: [Char]>
   |
   `- argument: example-argument :: [Char]
      |
      `- description: Takes the argument and prints it or not, depending on the mode
```

Okay! So we can now create programs which take arguments and options, so what
else do we want in a command line program? Flags! Lets add a flag to our
example program:

```haskell
main = command_
  . toplevel @"argument-taker"
  . optDef @"m" @"mode" "Print" $ \mode ->
    arg @"example-argument" $ \arg ->
    flag @"loud" $ \loud ->
    description @"Takes the argument and prints it or not, depending on the mode and possibly loudly" 
  . raw $ do
      let msg = if loud then map toUpper arg <> "!" else arg
      if mode == "Print" then putStrLn msg else pure ()
```

Running this with `argument-taker help`, we see:

```
usage:
name: argument-taker
|
+- subprogram: help
|
`- option: -m <mode :: [Char]>
   |
   `- argument: example-argument :: [Char]
      |
      `- flag: ~loud
         |
         `- description: Takes the argument and prints it or not, depending on the mode and possibly loudly
```

Okay, so we've added all of the normal command line things, but we haven't yet shown how to add a new command
to our program, so lets do that. To do this, we can write:

```haskell
main = command_
  . toplevel @"argument-taker"
  $ defaultProgram <+> sub @"shriek" (raw (putStrLn "AHHHHH!!"))
  where
  defaultProgram = 
      optDef @"m" @"mode" "Print" $ \mode ->
      arg @"example-argument" $ \arg ->
      flag @"loud" $ \loud ->
      description @"Takes the argument and prints it or not, depending on the mode and possibly loudly" 
    . raw $ do
        let msg = if loud then map toUpper arg <> "!" else arg
        if mode == "Print" then putStrLn msg else pure ()
```

Running this program with `argument-taker help`, we can see the docs yet again:

```
usage:
name: argument-taker
|
+- subprogram: help
|
+- option: -m <mode :: [Char]>
|  |
|  `- argument: example-argument :: [Char]
|     |
|     `- flag: ~loud
|        |
|        `- description: Takes the argument and prints it or not, depending on the mode and possibly loudly
|
`- subprogram: shriek
```

Awesome! So we have now shown how to use the primitives of CLI programs, as well as how to
add new subprograms. One more thing I would like to show that is different from normal CLI
libraries is that I added the ability to automatically search for environment variables and
pass them to your program. I just liked this, as sometimes when I use a CLI program I forget
this or that environment variable, and the documentation generation makes this self documenting
in commander-cli. We can add this to our program by writing:

```haskell
main = command_
  . toplevel @"argument-taker"
  $ env @"ARGUMENT_TAKER_DIRECTORY" \argumentTakerDirectory ->
      defaultProgram argumentTakerDirectory
  <+> sub @"shriek" (raw $ do
        setCurrentDirectory argumentTakerDirectory 
        putStrLn "AHHH!"
      )
  where
  defaultProgram argumentTakerDirectory = 
      optDef @"m" @"mode" "Print" $ \mode ->
      arg @"example-argument" $ \arg ->
      flag @"loud" $ \loud ->
      description @"Takes the argument and prints it or not, depending on the mode and possibly loudly" 
    . raw $ do
        setCurrentDirectory argumentTakerDirectory
        let msg = if loud then map toUpper arg <> "!" else arg
        if mode == "Print" then putStrLn msg else pure ()
```

Now, we will see `argument-taker help` as:

```
usage:
name: argument-taker
|
+- subprogram: help
|
`- required env: ARGUMENT_TAKER_DIRECTORY :: [Char]
   |
   +- option: -m <mode :: [Char]>
   |  |
   |  `- argument: example-argument :: [Char]
   |     |
   |     `- flag: ~loud
   |        |
   |        `- description: Takes the argument and prints it or not, depending on the mode and possibly loudly
   |
   `- subprogram: shriek
```

We can see that it documents the usage of this environment variable in a
reasonable way, but its not clear where exactly what it does exactly. First,
you might think to use the `description` combinator, but it isn't exactly made
for describing an input, but for documenting a path of a program. We can fix this
using the `annotated` combinator, which was made for describing inputs to our
program:

```haskell
main :: IO ()
main = command_
  . toplevel @"argument-taker"
  . annotated @"the directory we will go to for the program"
  $ env @"ARGUMENT_TAKER_DIRECTORY" \argumentTakerDirectory ->
      defaultProgram argumentTakerDirectory
  <+> sub @"shriek" (raw $ do
        setCurrentDirectory argumentTakerDirectory 
        putStrLn "AHHH!"
      )
  where
  defaultProgram argumentTakerDirectory = 
      optDef @"m" @"mode" "Print" $ \mode ->
      arg @"example-argument" $ \arg ->
      flag @"loud" $ \loud ->
      description @"Takes the argument and prints it or not, depending on the mode" 
    . raw $ do
        setCurrentDirectory argumentTakerDirectory
        let msg = if loud then map toUpper arg <> "!" else arg
        if mode == "Print" then putStrLn msg else pure ()
```

Running `argument-taker help` will result in:

```
usage:
name: argument-taker
|
+- subprogram: help
|
`- required env: ARGUMENT_TAKER_DIRECTORY :: [Char], the directory we will go to for the program
   |
   +- option: -m <mode :: [Char]>
   |  |
   |  `- argument: example-argument :: [Char]
   |     |
   |     `- flag: ~loud
   |        |
   |        `- description: Takes the argument and prints it or not, depending on the mode
   |
   `- subprogram: shriek
```

## Design

The library is based around the following classes:

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
  documentation :: Forest String
```

Instances of this class will define a syntactic element, a new instance of the
data family ProgramT, as well as its semantics in terms of the CommanderT monad,
which is something like a free backtracking monad. Users should not have to make
instances of this class, as the common CLI elements are already defined as
instances. Of course, you can if you want to, and it can be profitable to do so.

## Similar Projects

- [ReadArgs](https://hackage.haskell.org/package/ReadArgs) Simple command line argument parsing
- [argparser](https://hackage.haskell.org/package/argparser) Command line parsing framework for console applications
- [cli-extras](https://hackage.haskell.org/package/cli-extras) Miscellaneous utilities for building and working with command line interfaces
- [cli](https://hackage.haskell.org/package/cli) CLI
- [cmdargs](https://hackage.haskell.org/package/cmdargs) Command line argument processing
- [cmdtheline](https://hackage.haskell.org/package/cmdtheline) Declarative command-line option parsing and documentation library.
- [configifier](https://hackage.haskell.org/package/configifier) parser for config files, shell variables, command line args.
- [configuration-tools](https://hackage.haskell.org/package/configuration-tools) Tools for specifying and parsing configurations
- [console-program](https://hackage.haskell.org/package/console-program) Interpret the command line and a config file as commands and options
- [getopt-generics](https://hackage.haskell.org/package/getopt-generics) Create command line interfaces with ease
- [hflags](https://hackage.haskell.org/package/hflags) Command line flag parser, very similar to Google's gflags
- [multiarg](https://hackage.haskell.org/package/multiarg) Command lines for options that take multiple arguments
- [options](https://hackage.haskell.org/package/options) A powerful and easy-to-use command-line option parser.
- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) Utilities and combinators for parsing command line options
- [parseargs](https://hackage.haskell.org/package/parseargs) Parse command-line arguments
- [shell-utility](https://hackage.haskell.org/package/shell-utility) Utility functions for writing command-line programs
- [symantic-cli](https://hackage.haskell.org/package/symantic-cli) Symantics for parsing and documenting a CLI

