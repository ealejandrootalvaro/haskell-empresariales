{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.OrderRestaurant where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics


data OrderRestaurant = OrderRestaurant {id_order_restaurant :: Maybe Int
                         , dish  :: Maybe Int
                         , amount :: Maybe Int
                         , tipo :: Maybe Int
                         , delivery :: Maybe Int
                         , reservation :: Maybe Int
                         , price :: Maybe Double}
                         deriving (Show, Generic)

instance ToJSON OrderRestaurant
instance FromJSON OrderRestaurant

instance FromRow OrderRestaurant where
  fromRow = OrderRestaurant <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow OrderRestaurant where
  toRow  d = [ toField (id_order_restaurant d)
             , toField (dish d)
             , toField (amount d)
             , toField (tipo d)
             , toField (delivery d)
             , toField (reservation d)
             , toField (price d)
             ]
