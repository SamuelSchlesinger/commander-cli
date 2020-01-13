import Commander
import System.Process

main :: IO ()
main = command_ . toplevel @"start-postgres" $ raw do
  callCommand "pg_ctl -D /usr/local/var/postgres start && brew services start postgresql"
