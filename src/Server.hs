{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where

import           Servant.Server
import           Data.Data
import           Database (localConnString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Server.Recipes (recipesServer, RecipeRoutes)
import           Servant.API.Generic
import           Server.Ingredients

type KurkumaAPI = NamedRoutes NamedAPI

data NamedAPI mode =
  KurkumaRoutes { recipes :: mode :- NamedRoutes RecipeRoutes
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