import Commander
import System.Process

main :: IO ()
main = command_ . toplevel @"bad" $ raw $ callProcess "say" ["feels bad man"]
