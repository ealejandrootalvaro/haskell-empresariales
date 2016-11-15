{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.DbDelivery where

import Domain
import qualified Entities.Client as Client
import qualified Entities.Dish as Dish
import qualified Entities.Restaurant as Restaurant
import qualified Entities.Delivery as Delivery
import qualified Domain as Domain
import qualified Entities.OrderRestaurant as OrderRestaurant
import qualified Database.PostgreSQL.Simple as D

insertDelivery conn delivery= do
  let insert_query = "INSERT INTO delivery(address, phone,delivery_user,delivery_status) VALUES (?,?,?,?) returning id_delivery"
  result  <- D.query conn insert_query ((Delivery.address delivery),(Delivery.phone delivery),(Delivery.user_restaurant delivery),(False ::Bool)) :: IO [MyInt]
  return (result)

insertOrderRestaurant conn order= do
  let query="insert into order_restaurant(dish,amount,type,delivery,price) values (?,?,?,?,?)"
  result <- D.execute conn query ((OrderRestaurant.dish order),(OrderRestaurant.amount order), (OrderRestaurant.tipo order), (OrderRestaurant.delivery order), (OrderRestaurant.price order))
  return result
