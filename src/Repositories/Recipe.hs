{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Repositories.Recipe where

import           Data.Text
import           Database.Persist
import           Database
import           Data.Int
import           Database.Persist.Postgresql
import           Schema

findRecipeWithTitle :: PGInfo -> Text -> IO (Maybe (Entity Recipe))
findRecipeWithTitle info title =
  runAction info $ selectFirst [RecipeTitle ==. title] []

selectRecipes :: PGInfo -> IO [Entity Recipe]
selectRecipes conn = runAction conn $ selectList [] []

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