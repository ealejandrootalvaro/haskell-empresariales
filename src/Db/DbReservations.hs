{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.DbReservations where

import Domain

import qualified Entities.Dish as Dish
import qualified Domain as Domain
import qualified Entities.Reservations as Reservations
import qualified Database.PostgreSQL.Simple as D


------------------------------- RESERVATION -------------------------------


getReservationsByDate conn rango = do
    result <- (D.query conn "select id_reservation, user_restaurant,table_restaurant, date_init AT TIME ZONE 'MST', date_end AT TIME ZONE 'MST', amount_people, state from reservation where date_init >= ? and date_init <= ? and state = 4 " ((Reservations.fromInitialDate rango),(Reservations.toFinalDate rango)) :: IO [Reservations.Reservation])
    return result
