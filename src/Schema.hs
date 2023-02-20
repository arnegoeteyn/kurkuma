{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Schema where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Database.Persist
import           Database.Persist.Postgresql
import qualified Database.Persist.TH         as PTH
import           GHC.Generics                (Generic)

PTH.share
    [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
    [PTH.persistLowerCase|

    Recipe sql=recipes
        title Text
        description Text
        deriving Show Read Generic

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
    toJSON (Entity recipeId recipe) =
        object
            [ "id" .= recipeId
            , "title" .= recipeTitle recipe
            , "description" .= recipeDescription recipe
            ]

instance FromJSON Recipe

-- Ingredient
instance ToJSON (Entity Ingredient) where
    toJSON (Entity ingredientId ingredient) =
        object
            [ "id" .= ingredientId
            , "name" .= ingredientName ingredient
            , "category" .= ingredientCategory ingredient
            ]

instance FromJSON Ingredient where
    parseJSON = withObject "Ingredient" parseIngredient

parseIngredient :: Object -> Parser Ingredient
parseIngredient o = do
    iName <- o .: "name"
    iCategory <- o .: "category"
    return Ingredient{ingredientName = iName, ingredientCategory = iCategory}

data PutRecipeIngredient
    = ExistingIngredient IngredientId
    | NewIngredient Ingredient
    deriving (Show)

instance FromJSON PutRecipeIngredient where
    parseJSON (Object v) = do
        maybeId :: Maybe IngredientId <- v .:? "id"
        case maybeId of
            Just ingredientId -> return $ ExistingIngredient ingredientId
            Nothing           -> NewIngredient <$> parseIngredient v
    parseJSON _ = undefined

instance ToJSON PutRecipeIngredient where
    toJSON (ExistingIngredient ingredientId) = object ["id" .= ingredientId]
    toJSON (NewIngredient ingredient) =
        object
            [ "name" .= ingredientName ingredient
            , "category" .= ingredientCategory ingredient
            ]
