{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.Dish where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

-- Datatypes que representan la base de datos
data Dish_type = Dish_type {id_dish_type :: Maybe Int,
                            name_dish_type :: Maybe String}
                            deriving (Show,Generic)

instance ToJSON Dish_type
instance FromJSON Dish_type

data Dish = Dish {                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  id_dish :: Maybe Int,
                  name_dish :: Maybe String,
                  description :: Maybe String,
                  price :: Maybe Double,
                  restaurant :: Maybe Int,
                  type_dish :: Maybe Int }
                  deriving (Show,Generic)

instance ToJSON Dish
instance FromJSON Dish


instance FromRow Dish_type where
  fromRow = Dish_type <$> field <*> field

instance ToRow Dish_type where
  toRow  d = [toField (id_dish_type d),
             toField (name_dish_type d)
             ]


instance FromRow Dish where
  fromRow = Dish <$> field <*> field <*> field <*> field <*> field <*> field


instance ToRow Dish where
  toRow d = [toField (id_dish d),
            toField (name_dish d),
            toField (description d),
            toField (price d),
            toField (restaurant d),
            toField (type_dish d)
            ]
