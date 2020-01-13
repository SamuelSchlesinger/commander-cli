import Commander
import System.Process

main :: IO ()
main = command_ . toplevel @"stop-postgres" $ raw do
  callCommand "pg_ctl -D /usr/local/var/postgres stop && brew services stop postgresql"
