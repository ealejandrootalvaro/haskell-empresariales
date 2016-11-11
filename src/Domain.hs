{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import GHC.Generics

data Resultado= Resultado{tipo :: Maybe String, mensaje :: Maybe String} |
                ResultadoConCampos {tipo :: Maybe String, mensaje :: Maybe String, campos :: Maybe [String]}
                deriving (Show,Generic)
instance ToJSON Resultado
