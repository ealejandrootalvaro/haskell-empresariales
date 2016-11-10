{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

data Resultado= Resultado{tipo :: Maybe String, mensaje :: Maybe String} deriving (Show,Generic)
instance ToJSON Resultado

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

data Restaurant = Restaurant{id_restaurant :: Maybe Int,
                             name_restaurant :: Maybe String,
                             description_restaurant :: Maybe String,
                             email_restaurant :: Maybe String,
                             admin :: Maybe Int}
                             deriving (Show,Generic)
                          
instance ToJSON Restaurant
instance FromJSON Restaurant 

data Client= Client{id_user :: Maybe Int,
                    username :: Maybe String,
                    email :: Maybe String,
                    password :: Maybe String,
                    nameUser :: Maybe String,
                    role :: Maybe Int,
                    token :: Maybe String,
                    phone :: Maybe String,
                    balance :: Maybe Int,
                    identification :: Maybe String
                     }
                    deriving (Show,Generic)

-- Poder convertir de datatype a JSON
instance ToJSON Client
instance FromJSON Client


-- Para poder convertir de fila de base de datos a datatype y viceversa
instance FromRow Client where
  fromRow = Client <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Client where
  toRow d = [toField (id_user d),
             toField (username d),
             toField (email d),
             toField (password d),
             toField (nameUser d),
             toField (role d),
             toField (token d),
             toField (phone d),
             toField (balance d),
             toField (identification d)
             ]

instance FromRow Restaurant where
  fromRow = Restaurant <$> field <*> field <*> field <*> field <*> field 

instance ToRow Restaurant where
  toRow d = [toField (id_restaurant d),
             toField (name_restaurant d),
             toField (description_restaurant d),
             toField (email_restaurant d),
             toField (admin d)
             ]

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
