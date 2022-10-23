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
{-# LANGUAGE ScopedTypeVariables #-}

module Schema where

import qualified Database.Persist.TH as PTH
import           Data.Text
import           Data.Aeson
import           Database.Persist
import           Data.Aeson.Types
import           Database.Persist.Postgresql
import           Data.Aeson.Encoding
import qualified Data.Aeson.Parser

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|

    Recipe sql=recipes
        title Text
        description Text
        deriving Show Read

    Ingredient sql=ingredients
        name Text
        category Text
        deriving Show Read

    RecipeIngredients sql=recipe_ingredients
        recipe RecipeId
        ingredient IngredientId
        amount Text
|]

-- Recipe
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

-- Ingredient
instance ToJSON (Entity Ingredient) where
  toJSON (Entity ingredientId ingredient) = object
    [ "id" .= ingredientId
    , "name" .= ingredientName ingredient
    , "category" .= ingredientCategory ingredient]

instance FromJSON Ingredient where
  parseJSON = withObject "Ingredient" parseIngredient

parseIngredient :: Object -> Parser Ingredient
parseIngredient o = do
  iName <- o .: "name"
  iCategory <- o .: "category"
  return Ingredient { ingredientName = iName, ingredientCategory = iCategory }

data PutRecipeIngredient = ExistingIngredient IngredientId
                         | NewIngredient Ingredient
  deriving (Show)

instance FromJSON PutRecipeIngredient where
  --   parseJSON (Number ingredientId) = pure
  --     (fmap ExistingIngredient <$> (parseJSON ingredientId))
  --   parseJSON ingredient =
  --     withObject "Ingredient" (fmap NewIngredient <$> parseIngredient) ingredient
  --   parseJSON (Object v) = case v .:? "id" of 
  --     Just (Object h) -> ExistingIngredient <$> (h .: "id")
  --     Just _          -> undefined
  --     Nothing         -> NewIngredient <$> parseIngredient v
  --   parseJSON _ = undefined
  parseJSON (Object v) = do
    maybeId :: Maybe IngredientId <- v .:? "id"
    case maybeId of
      Just ingredientId -> return $ ExistingIngredient ingredientId
      Nothing           -> NewIngredient <$> parseIngredient v
  parseJSON _ = undefined