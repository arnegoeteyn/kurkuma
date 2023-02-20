{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeApplications      #-}

module Server where

import           Data.Data
import           Database                 (localConnString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.API.Generic
import           Servant.Server
import           Server.Ingredients
import           Server.Recipes           (RecipeRoutes, recipesServer)

type KurkumaAPI = NamedRoutes NamedAPI

data NamedAPI mode =
  KurkumaRoutes { recipes     :: mode :- NamedRoutes RecipeRoutes
                , ingredients :: mode :- NamedRoutes IngredientRoutes
                }
  deriving Generic

server :: Server KurkumaAPI
server = KurkumaRoutes { recipes = recipesServer ctx
                       , ingredients = ingredientsServer ctx
                       }
  where
    ctx = localConnString

app :: Application
app = serve (Proxy @KurkumaAPI) server

runServer :: IO ()
runServer = do
  Prelude.putStrLn "Ready to go"
  run 8000 app
