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

type RecipesAPI = "recipes" :> Get '[JSON] [Entity Recipe]
  :<|> "recipes" :> ReqBody '[JSON] Recipe :> Post '[JSON] Int64

recipesAPI :: Proxy RecipesAPI
recipesAPI = Proxy :: Proxy RecipesAPI

recipesServer :: PGInfo -> Server RecipesAPI
recipesServer info = getRecipesHandler info :<|> createRecipeHandler info

getRecipesHandler :: PGInfo -> Handler [Entity Recipe]
getRecipesHandler conn = liftIO $ selectRecipes conn

createRecipeHandler :: PGInfo -> Recipe -> Handler Int64
createRecipeHandler conn recipe = do
  duplicate <- liftIO $ findRecipeWithTitle conn $ recipeTitle recipe
  case duplicate of
    Nothing -> liftIO $ createRecipe conn recipe
    Just _  -> Handler
      (throwError
       $ err401 { errBody = "A recipe with that name already exists" })

-- createRecipe conn recipe
runServer :: IO ()
runServer = run 8000 (serve recipesAPI (recipesServer localConnString))