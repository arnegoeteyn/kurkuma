{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Repositories.Ingredient where

import           Database.Persist
import           Database.Persist.Postgresql
import           Database (PGInfo, runAction)
import           Database.Esqueleto.Experimental as E
import           Control.Monad.Logger (LoggingT)
import           Data.Text (Text)
import           Schema (Ingredient, EntityField(IngredientName))

createIngredient
  :: PGInfo -> Ingredient -> IO (Database.Persist.Postgresql.Key Ingredient)
createIngredient conn ingredient = runAction conn (insert ingredient)

getDuplicateIngredients :: PGInfo -> IO [(Int, Text)]
getDuplicateIngredients
  conn = runAction conn (map m <$> getDuplicateIngredientsStmt)
  where
    m (a, b) = (E.unValue a, E.unValue b)

getDuplicateIngredientsStmt
  :: SqlPersistT (LoggingT IO) [(E.Value Int, E.Value Text)]
getDuplicateIngredientsStmt = E.select
  $ do
    ingredients <- E.from $ E.table @Ingredient
    E.groupBy $ ingredients E.^. IngredientName
    E.having $ E.countRows E.>. E.val 1
    pure (E.countRows, ingredients E.^. IngredientName)