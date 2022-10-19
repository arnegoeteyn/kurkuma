module Main (main) where

import           Server (runServer)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Migration (migrateDB)
import           Database (localConnString)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-s"] = server
parse ["-m"] = migrate
parse _ = exitFailure

server :: IO ()
server = runServer

migrate :: IO ()
migrate = migrateDB localConnString

