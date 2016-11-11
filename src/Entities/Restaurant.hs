
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.Restaurant where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics



data Restaurant = Restaurant{id_restaurant :: Maybe Int,
                             name :: Maybe String,
                             description :: Maybe String,
                             email :: Maybe String,
                             admin :: Maybe Int}
                             deriving (Show,Generic)
                          
instance ToJSON Restaurant
instance FromJSON Restaurant 


instance FromRow Restaurant where
  fromRow = Restaurant <$> field <*> field <*> field <*> field <*> field 

instance ToRow Restaurant where
  toRow d = [toField (id_restaurant d),
             toField (name d),
             toField (description d),
             toField (email d),
             toField (admin d)
             ]
