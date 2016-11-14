{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.Delivery where

import Data.Aeson
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Entities.OrderRestaurant

data Delivery  = Delivery { id_delivery :: Maybe Int
                        , address  :: Maybe String
                        , phone :: Maybe String
                        , user_restaurant :: Maybe Int
                        , delivery_status :: Maybe Int
                        , score :: Maybe Int
                        , comment :: Maybe String
                        , orders  :: [OrderRestaurant]
                        }  deriving (Generic, Show)

instance ToJSON Delivery
instance FromJSON Delivery

instance ToRow Delivery where
  toRow  d = [ toField (id_delivery d)
             , toField (address d)
             , toField (phone d)
             , toField (user_restaurant d)
             , toField (delivery_status d)
             , toField (score d)
             , toField (comment d)
             ]
