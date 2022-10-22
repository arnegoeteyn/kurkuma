{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Servant.API
import           Servant.Server
import           Database.Persist.Postgresql
import           Data.Data
import           Database (localConnString, PGInfo)
import           Network.Wai.Handler.Warp (run)
import           Control.Monad.Cont
import           Data.Int (Int64)
import           Repositories.Recipe
import           Servant (throwError)
import           Schema

type RecipesAPI =
  GetRecipes :<|> GetRecipe :<|> PostRecipe :<|> PutRecipeIngredients

type GetRecipes = "recipes" :> Get '[JSON] [Entity Recipe]

type GetRecipe = "recipes" :> Capture "recipeId" RecipeId
  :> Get '[JSON] (Entity Recipe)

type PostRecipe = "recipes" :> ReqBody '[JSON] Recipe :> Post '[JSON] Int64

type PutRecipeIngredients = "recipes" :> Capture "userId" (Key Recipe)
  :> "ingredients"
  :> ReqBody '[JSON] [IngredientId] :> Put '[JSON] [Key RecipeIngredients]

recipesAPI :: Proxy RecipesAPI
recipesAPI = Proxy :: Proxy RecipesAPI

recipesServer :: PGInfo -> Server RecipesAPI
recipesServer info = getRecipesHandler info
  :<|> getRecipeHandler info
  :<|> createRecipeHandler info
  :<|> putRecipeHandler info

getRecipesHandler :: PGInfo -> Handler [Entity Recipe]
getRecipesHandler conn = liftIO $ selectRecipes conn

getRecipeHandler :: PGInfo -> RecipeId -> Handler (Entity Recipe)
getRecipeHandler recipeId conn = do
  maybeRecipe <- liftIO $ selectRecipe recipeId conn
  case maybeRecipe of
    Just recipe -> return recipe
    Nothing     -> Handler
      (throwError $ err404 { errBody = "A recipe with that id can't be found" })

createRecipeHandler :: PGInfo -> Recipe -> Handler Int64
createRecipeHandler conn recipe = do
  duplicate <- liftIO $ findRecipeWithTitle conn $ recipeTitle recipe
  case duplicate of
    Nothing -> liftIO $ createRecipe conn recipe
    Just _  -> Handler
      (throwError
       $ err401 { errBody = "A recipe with that name already exists" })

putRecipeHandler
  :: PGInfo -> RecipeId -> [IngredientId] -> Handler [Key RecipeIngredients]
putRecipeHandler conn recipe ingredients =
  liftIO $ setRecipeIngredients conn recipe ingredients

runServer :: IO ()
runServer = run 8000 (serve recipesAPI (recipesServer localConnString))