{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Database.Persist.Postgresql

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString =
  "host=127.0.0.1 port=5432 user=postgres dbname=kurkuma password=postgres"

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = runStdoutLoggingT
  $ filterLogger logFilter
  $ withPostgresqlConn connectionString
  $ \backend -> runReaderT action backend

-- migrateDB :: PGInfo -> IO ()
-- migrateDB connString = runAction connString (runMigration migrateAll)
logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug = False
logFilter _ (LevelOther _) = False
