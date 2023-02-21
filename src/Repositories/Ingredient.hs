{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}

module Repositories.Ingredient where

import Control.Monad.Logger (LoggingT)
import Data.Int (Int64)
import Data.Text (Text)
import Database (PGInfo, runAction)
import Database.Esqueleto.Experimental as E
import Database.Persist.Postgresql as P
import Schema (EntityField (IngredientId, IngredientName, RecipeIngredientsIngredient), Ingredient)

createIngredient ::
  PGInfo -> Ingredient -> IO (P.Key Ingredient)
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
  ents <- duplicates key
  let (earliestKey, tail_) = firstIngredient ents
  case earliestKey of
    Just x -> do
      mapM_ (mergeDuplicatesStmt x) tail_
      mapM_ deleteDuplicatesStmt tail_
    Nothing -> return ()
  return $ fmap fromSqlKey earliestKey
  where
    firstIngredient (x : xs) = (Just (entityKey x), map entityKey xs)
    firstIngredient _ = (Nothing, [])

duplicates :: Text -> SqlPersistT (LoggingT IO) [Entity Ingredient]
duplicates key = selectList [IngredientName P.==. key] []

mergeDuplicatesStmt :: Key Ingredient -> Key Ingredient -> SqlPersistT (LoggingT IO) ()
mergeDuplicatesStmt to_ from_ = do
  E.update $ \ri -> do
    E.set ri [RecipeIngredientsIngredient E.=. val to_]
    E.where_ (ri E.^. RecipeIngredientsIngredient E.==. val from_)

deleteDuplicatesStmt :: Key Ingredient -> SqlPersistT (LoggingT IO) ()
deleteDuplicatesStmt from_ = E.delete $ do
    i <- from $ table @Ingredient
    E.where_ (i E.^. IngredientId E.==. val from_)
