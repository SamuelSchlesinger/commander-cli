# Commander CLI

[![Hackage](https://img.shields.io/hackage/v/commander-cli.svg)](https://hackage.haskell.org/package/commander-cli)
[![Build Status](https://travis-ci.org/SamuelSchlesinger/commander-cli.svg?branch=master)](https://travis-ci.org/SamuelSchlesinger/commander-cli)

This library is meant to allow Haskell programs to quickly and easily construct
command line interfaces which are easy to use, especially as a Haskell user. To
learn, I suggest viewing/playing with the task-manager application which
comes with this repository. Here, we'll display a simpler example:

```haskell
main = command_ . toplevel @"argument-taker" . arg @"example-argument" $ raw . putStrLn
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

The meaning of this is that every path in the tree is a unique command. The one
we've used is the help command. If we run this program with `argument-taker hello`
we will see:

```
hello
```

Okay, so we've made a program with hardly any scaffolding that gives us a
decent help message, and pipes through our argument correctly. Naturally, we
might want to expand on the documentation of this program, as its not quite
obvious enough what it does.

```
main = command_ . toplevel @"argument-taker" . arg @"example-argument" $ (description @"Takes the argument and prints it" . raw . putStrLn)
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

```
main = command_ . toplevel @"argument-taker" $
  opt @"m" @"mode" \mode ->
    arg @"example-argument" $ \arg ->
      description @"Takes the argument and prints it or not, depending on the mode" . raw $ do
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
