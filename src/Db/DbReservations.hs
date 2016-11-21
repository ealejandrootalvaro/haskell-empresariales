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
    
insertReservation conn reservacion = do 
   result <- D.execute conn "insert into reservation (id_reservation, user_restaurant, table_restaurant, date_init, date_end, amount_people, state) values (?,?,?,?,?,?,?)" ((Reservations.id_reservation reservacion),(Reservations.user reservacion),(Reservations.restaurant reservacion),(Reservations.initialDate reservacion),(Reservations.finalDate reservacion),(Reservations.guests reservacion),(Reservations.state reservacion))
   return result


getDishByReservations conn rango = do
    result <- (D.query conn "select dish.id_dish, dish.name_dish, dish.description, dish.price, dish.restaurant, dish.type from dish, order_restaurant, reservation where reservation.date_init >= ? and reservation.date_init <= ? and reservation.state = 4 and reservation.id_reservation = order_restaurant.reservation and order_restaurant.dish = dish.id_dish and order_restaurant.type = 1" ((Reservations.fromInitialDate rango),(Reservations.toFinalDate rango)) :: IO [Dish.Dish])
    return result
--    list <- D.execute conn "select * from reservation where date_init between ? and ?" ((Reservations.from rango),(Reservations.to rango))
