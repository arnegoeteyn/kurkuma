{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.Ingredients where

import           Servant.API.Generic
import           Servant
import           Data.Text
import           Database
import           Servant.Server.Generic
import           Control.Monad.Cont
import           Repositories.Ingredient

newtype IngredientRoutes mode =
  IngredientRoutes { duplicate :: mode :- "ingredients"
                               :> "duplicate" :> Get '[JSON] [(Int, Text)]
                   }
  deriving Generic

ingredientsServer :: PGInfo -> IngredientRoutes AsServer
ingredientsServer
  ctx = IngredientRoutes { duplicate = duplicateIngredientsHandler ctx }

duplicateIngredientsHandler :: PGInfo -> Handler [(Int, Text)]
duplicateIngredientsHandler conn = liftIO $ getDuplicateIngredients conn