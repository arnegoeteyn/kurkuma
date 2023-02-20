{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}

module Repositories.Ingredient where

import Control.Monad.Logger (LoggingT)
import Data.Int (Int64)
import Data.Text (Text)
import Database (PGInfo, runAction)
import Database.Esqueleto.Experimental as E
import Database.Persist.Postgresql
import Schema (EntityField (IngredientId, IngredientName), Ingredient, Key (IngredientKey))

createIngredient ::
  PGInfo -> Ingredient -> IO (Database.Persist.Postgresql.Key Ingredient)
createIngredient conn ingredient = runAction conn (insert ingredient)

getDuplicateIngredients :: PGInfo -> IO [(Int, Text)]
getDuplicateIngredients
  conn = runAction conn (map m <$> getDuplicateIngredientsStmt)
    where
      m (a, b) = (E.unValue a, E.unValue b)

getDuplicateIngredientsStmt ::
  SqlPersistT (LoggingT IO) [(E.Value Int, E.Value Text)]
getDuplicateIngredientsStmt = E.select $
  do
    ingredients <- E.from $ E.table @Ingredient
    E.groupBy $ ingredients E.^. IngredientName
    E.having $ E.countRows E.>. E.val 1
    pure (E.countRows, ingredients E.^. IngredientName)

mergeDuplicateIngredients :: PGInfo -> Text -> IO (Maybe Int64)
mergeDuplicateIngredients conn key = runAction conn $ do
  earliestKey <- earliestDuplicateStmt key
  let z = Prelude.map (fmap fromSqlKey) earliestKey
  r z
  where
    r (x : _) = return $ Just (E.unValue x)
    r _ = return Nothing

earliestDuplicateStmt :: Text -> SqlPersistT (LoggingT IO) [E.Value (Key Ingredient)]
earliestDuplicateStmt key = E.select $ do
  do
    ingredients <- E.from $ E.table @Ingredient
    E.where_ (ingredients E.^. IngredientName E.==. val key)
    let min' = ingredients E.^. persistIdField
    pure min'

mergeDuplicatesStmt :: String -> SqlPersistT (LoggingT IO) Int64
mergeDuplicatesStmt key = undefined
