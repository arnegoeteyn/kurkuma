{-# LANGUAGE TemplateHaskell #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Repositories.Ingredient where

import qualified Database.Persist.TH as PTH
import           Data.Text
import           Data.Aeson
import           Database.Persist
import           Data.Aeson.Types
import           Database.Persist.Postgresql
import           Database (PGInfo, runAction)
import           Data.Int (Int64)
import           Schema (Ingredient(Ingredient))

createIngredient
  :: PGInfo -> Ingredient -> IO (Database.Persist.Postgresql.Key Ingredient)
createIngredient conn ingredient = runAction conn (insert ingredient)