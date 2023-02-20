{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Ingredients where

import Control.Monad.Cont
import Data.Int (Int64)
import Data.Text
import Database
import Repositories.Ingredient
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data IngredientRoutes mode = IngredientRoutes
  { duplicate ::
      mode
        :- "ingredients"
        :> "duplicate"
        :> Get '[JSON] [(Int, Text)],
    mergeDuplicates ::
      mode
        :- "ingredients"
        :> Capture "ingredientName" Text
        :> "merge"
        :> Put '[JSON] (Maybe Int64)
  }
  deriving (Generic)

ingredientsServer :: PGInfo -> IngredientRoutes AsServer
ingredientsServer
  conn =
    IngredientRoutes
      { duplicate = duplicateIngredientsHandler conn,
        mergeDuplicates = mergeDuplicateHandler conn
      }

duplicateIngredientsHandler :: PGInfo -> Handler [(Int, Text)]
duplicateIngredientsHandler conn = liftIO $ getDuplicateIngredients conn

mergeDuplicateHandler :: PGInfo -> Text -> Handler (Maybe Int64)
mergeDuplicateHandler conn key = liftIO $ mergeDuplicateIngredients conn key
