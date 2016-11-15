{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

data Resultado= Resultado{tipo :: Maybe String, mensaje :: Maybe String} |
                ResultadoConCampos {tipo :: Maybe String, mensaje :: Maybe String, campos :: Maybe [String]} |
                ResultadoDomiclio {tipo :: Maybe String, total :: Maybe String}
                deriving (Show,Generic)
instance ToJSON Resultado
--------------------------------------------------------------------------------

data MyInt= MyInt {int :: Int} deriving (Generic, Show)

getInt :: MyInt -> Int
getInt (MyInt int) = int

instance FromRow MyInt where
  fromRow = MyInt <$> field

instance ToRow MyInt where
  toRow  d = [ toField (int d) ]
--------------------------------------------------------------------------------

data MyFloat = MyFloat {float :: Double} deriving (Generic, Show)

getFloat :: MyFloat -> Double
getFloat (MyFloat float) = float

instance FromRow MyFloat where
  fromRow = MyFloat <$> field

instance ToRow MyFloat where
  toRow  d = [ toField (float d) ]
