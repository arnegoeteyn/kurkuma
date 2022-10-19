module Migration (migrateDB) where

import           Database (PGInfo, runAction)
import           Database.Persist.Postgresql (runMigration)
import           Schema (migrateAll)

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)