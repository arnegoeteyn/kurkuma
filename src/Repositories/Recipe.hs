{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Repositories.Recipe where

import Control.Monad.Logger
import Data.Int
import Data.Text
import Database
import Database.Esqueleto as E
import Database.Persist as P
import Database.Persist.Postgresql
import Schema

findRecipeWithTitle :: PGInfo -> Text -> IO (Maybe (Entity Recipe))
findRecipeWithTitle info title =
    runAction info $
        selectFirst [RecipeTitle Database.Persist.Postgresql.==. title] []

selectRecipes :: PGInfo -> IO [Entity Recipe]
selectRecipes conn = runAction conn $
    do
        monadLoggerLog
            loc
            (pack "")
            LevelInfo
            (toLogStr $ pack "All recipes requested")
        selectList [] []

selectRecipe ::
    PGInfo -> RecipeId -> IO (Maybe (Entity Recipe, [Entity Ingredient]))
selectRecipe conn recipeId = runAction conn $
    do
        recipe <-
            selectFirst [RecipeId Database.Persist.Postgresql.==. recipeId] []
        ingredients <- getRecipeIngredientsStmt recipeId
        return (fmap (\r -> (r, ingredients)) recipe)

createRecipe :: PGInfo -> Recipe -> IO Int64
createRecipe conn recipe = fromSqlKey <$> runAction conn (insert recipe)

deleteRecipe :: PGInfo -> RecipeId -> IO ()
deleteRecipe conn i = runAction conn $ P.delete i

setRecipeIngredients ::
    PGInfo -> RecipeId -> [PutRecipeIngredient] -> IO [Key RecipeIngredients]
setRecipeIngredients conn recipeId ingredients = runAction conn $
    do
        removeRecipeIngredientsStmt recipeId
        creates <- mapM insertIfNotExists ingredients
        mapM insert (recipeIngredients creates)
  where
    insertIfNotExists (NewIngredient ingredient) = insert ingredient
    insertIfNotExists (ExistingIngredient i) = return i

    recipeIngredients = Prelude.map (\i -> RecipeIngredients recipeId i "1")

getRecipeIngredients :: PGInfo -> RecipeId -> IO [Entity Ingredient]
getRecipeIngredients conn recipeId =
    runAction conn $ getRecipeIngredientsStmt recipeId

getRecipeIngredientsStmt ::
    RecipeId -> SqlPersistT (LoggingT IO) [Entity Ingredient]
getRecipeIngredientsStmt recipeId = select . from $
    \(ingredients `InnerJoin` recipeIngredients) -> do
        E.on $
            (ingredients ^. IngredientId)
                E.==. (recipeIngredients ^. RecipeIngredientsIngredient)
        where_ $ (recipeIngredients ^. RecipeIngredientsRecipe) E.==. val recipeId
        return ingredients

removeRecipeIngredientsStmt :: RecipeId -> SqlPersistT (LoggingT IO) ()
removeRecipeIngredientsStmt recipeId =
    deleteWhere
        [RecipeIngredientsRecipe Database.Persist.Postgresql.==. recipeId]

loc :: Loc
loc =
    Loc
        { loc_filename = "Recipe.hs"
        , loc_module = ""
        , loc_package = ""
        , loc_start = (0, 0)
        , loc_end = (0, 0)
        }