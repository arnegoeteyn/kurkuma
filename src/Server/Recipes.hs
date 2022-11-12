{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Recipes where

import Control.Monad.Cont
import Control.Monad.Error.Class
import Data.Int (Int64)
import Database (PGInfo)
import Database.Persist
import Repositories.Recipe
import Schema
import Servant
import Servant.API.Generic
import Servant.Server.Generic (AsServer)

data RecipeRoutes mode = RecipeRoutes
  { list :: mode :- "recipes" :> Get '[JSON] [Entity Recipe],
    find ::
      mode :- "recipes" :> Capture "recipeId" RecipeId
        :> Get '[JSON] (Entity Recipe, [Entity Ingredient]),
    create ::
      mode :- "recipes"
        :> ReqBody '[JSON] Recipe
        :> Post '[JSON] Int64,
    putRecipeIngredients ::
      mode :- "recipes" :> Capture "recipeId" RecipeId
        :> "ingredients"
        :> ReqBody '[JSON] [PutRecipeIngredient]
        :> Put '[JSON] [RecipeIngredientsId]
  }
  deriving (Generic)

recipesServer :: PGInfo -> RecipeRoutes AsServer
recipesServer ctx =
  RecipeRoutes
    { list = getRecipesHandler ctx,
      find = getRecipeHandler ctx,
      create = createRecipeHandler ctx,
      putRecipeIngredients = putRecipeHandler ctx
    }

getRecipesHandler :: PGInfo -> Handler [Entity Recipe]
getRecipesHandler conn = liftIO $ selectRecipes conn

getRecipeHandler ::
  PGInfo -> RecipeId -> Handler (Entity Recipe, [Entity Ingredient])
getRecipeHandler recipeId conn = do
  maybeRecipe <- liftIO $ selectRecipe recipeId conn
  case maybeRecipe of
    Just recipe -> return recipe
    Nothing ->
      Handler
        (throwError $ err404 {errBody = "A recipe with that id can't be found"})

createRecipeHandler :: PGInfo -> Recipe -> Handler Int64
createRecipeHandler conn recipe = do
  duplicate <- liftIO $ findRecipeWithTitle conn $ recipeTitle recipe
  case duplicate of
    Nothing -> liftIO $ createRecipe conn recipe
    Just _ ->
      Handler
        ( throwError $
            err401 {errBody = "A recipe with that name already exists"}
        )

putRecipeHandler ::
  PGInfo ->
  RecipeId ->
  [PutRecipeIngredient] ->
  Handler [RecipeIngredientsId]
putRecipeHandler conn recipe ingredients =
  liftIO $ setRecipeIngredients conn recipe ingredients
