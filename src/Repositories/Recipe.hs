{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Repositories.Recipe where

import           Data.Text
import           Database.Persist
import           Database
import           Data.Int
import           Database.Persist.Postgresql
import           Schema
import           Database.Esqueleto
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Cont

findRecipeWithTitle :: PGInfo -> Text -> IO (Maybe (Entity Recipe))
findRecipeWithTitle info title = runAction info
  $ selectFirst [RecipeTitle Database.Persist.Postgresql.==. title] []

selectRecipes :: PGInfo -> IO [Entity Recipe]
selectRecipes conn = runAction conn $ selectList [] []

selectRecipe
  :: PGInfo -> RecipeId -> IO (Maybe (Entity Recipe, [Entity Ingredient]))
selectRecipe conn recipeId = runAction conn
  $ do
    recipe
      <- selectFirst [RecipeId Database.Persist.Postgresql.==. recipeId] []
    ingredients <- liftIO $ getRecipeIngredients conn recipeId
    -- todo do notation cleaner hier
    return (fmap (\r -> (r, ingredients)) recipe)

createRecipe :: PGInfo -> Recipe -> IO Int64
createRecipe conn recipe = fromSqlKey <$> runAction conn (insert recipe)

setRecipeIngredients
  :: PGInfo -> RecipeId -> [IngredientId] -> IO [Key RecipeIngredients]
setRecipeIngredients conn recipeId ingredients =
  mapM (runAction conn . insert) recipeIngredients
  where
    recipeIngredients = Prelude.map
      (\i -> RecipeIngredients recipeId i "homemade bbi")
      ingredients

getRecipeIngredients :: PGInfo -> RecipeId -> IO [Entity Ingredient]
getRecipeIngredients conn recipeId = runAction conn action
  where
    action :: SqlPersistT (LoggingT IO) [Entity Ingredient]
    action = select . from
      $ \(ingredients `InnerJoin` recipeIngredients) -> do
        on
          $ (ingredients ^. IngredientId)
          Database.Esqueleto.==. (recipeIngredients
                                  ^. RecipeIngredientsIngredient)
        where_
          $ (recipeIngredients ^. RecipeIngredientsRecipe)
          Database.Esqueleto.==. val recipeId
        return ingredients
