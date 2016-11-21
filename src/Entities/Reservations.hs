{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.Reservations where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
--import Data.Time (UTCTime,LocalTime,ZonedTime)
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
--import Database.PostgreSQL.Simple.Time
import Data.Time
import GHC.Generics



data Reservation = Reservation{id_reservation :: Maybe Int,
                             user :: Maybe Int,
                             restaurant :: Maybe Int,
                             initialDate :: Maybe UTCTime,
                             finalDate ::  Maybe UTCTime,
                             guests :: Maybe Int,
                             state :: Maybe Int}
                             deriving (Show,Generic)
                          
instance ToJSON Reservation
instance FromJSON Reservation 

instance FromRow Reservation where
  fromRow = Reservation <$> field <*> field <*> field <*> field <*> field <*> field <*> field 

instance ToRow Reservation where
  toRow d = [toField (id_reservation d),
             toField (user d),
             toField (restaurant  d),
             toField (initialDate d),
             toField (finalDate d),
             toField (guests d),
             toField (state d)
             ]


data TimeRange = TimeRange{fromInitialDate :: Maybe UTCTime, toFinalDate :: Maybe UTCTime}
                           deriving (Show,Generic)
instance ToJSON TimeRange
instance FromJSON TimeRange
