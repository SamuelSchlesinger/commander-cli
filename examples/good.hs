import Commander
import System.Process

main :: IO ()
main = command_ . toplevel @"good" $ raw $ callProcess "say" ["feels good man"]
