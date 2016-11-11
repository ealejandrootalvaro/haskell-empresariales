{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.Client where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

data Client= Client{id_user :: Maybe Int,
                    username :: Maybe String,
                    email :: Maybe String,
                    password :: Maybe String,
                    name :: Maybe String,
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
             toField (name d),
             toField (role d),
             toField (token d),
             toField (phone d),
             toField (balance d),
             toField (identification d)
             ]
