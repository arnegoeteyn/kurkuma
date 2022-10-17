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

import qualified Database.Persist.TH as PTH
import           Data.Text
import           Data.Aeson
import           Database.Persist
import           Data.Aeson.Types
import           Database
import           Data.Int
import           Database.Persist.Postgresql

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
  Recipe sql=recipes
    title Text
    description Text
    deriving Show Read
|]

instance ToJSON (Entity Recipe) where
  toJSON (Entity recipeId recipe) = object
    [ "id" .= recipeId
    , "title" .= recipeTitle recipe
    , "description" .= recipeDescription recipe]

instance FromJSON Recipe where
  parseJSON = withObject "Recipe" parseRecipe

parseRecipe :: Object -> Parser Recipe
parseRecipe o = do
  rTitle <- o .: "title"
  rDescription <- o .: "description"
  return Recipe { recipeTitle = rTitle, recipeDescription = rDescription }

findRecipeWithTitle :: PGInfo -> Text -> IO (Maybe (Entity Recipe))
findRecipeWithTitle info title =
  runAction info $ selectFirst [RecipeTitle ==. title] []

selectRecipes :: PGInfo -> IO [Entity Recipe]
selectRecipes conn = runAction conn $ selectList [] []

createRecipe :: PGInfo -> Recipe -> IO Int64
createRecipe conn recipe = fromSqlKey <$> runAction conn (insert recipe)