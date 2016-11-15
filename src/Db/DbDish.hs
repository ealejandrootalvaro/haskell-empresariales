{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.DbDish where

import Domain
import qualified Entities.Client as Client
import qualified Entities.Dish as Dish
import qualified Entities.Restaurant as Restaurant
import qualified Entities.Delivery as Delivery
import qualified Domain as Domain
import qualified Entities.OrderRestaurant as OrderRestaurant
import qualified Database.PostgreSQL.Simple as D

getAllMenus :: D.Connection -> IO [Dish.Dish]
getAllMenus c = do
  list <- (D.query_ c "select * from dish" :: IO [Dish.Dish])
  return list

getPricesDish conn order=do
  price <- (D.query conn "select price from dish where id_dish = ?" (D.Only  $ OrderRestaurant.dish order) :: IO [MyFloat] )
  return price

getMenuById :: D.Connection -> Integer -> IO [Dish.Dish]
getMenuById conn int = do
    menu <- (D.query conn "select * from dish where id_dish = ?" (D.Only int) :: IO [Dish.Dish])
    return menu

insertMenu conn menu = do
    result <- D.execute conn "insert into dish (name_dish,description,price,restaurant,type) values (?,?,?,?,?)" ((Dish.name_dish menu), (Dish.description menu), (Dish.price menu),(Dish.restaurant menu),(Dish.type_dish menu))
    return result

updateMenu conn menu = do
    result <- D.execute conn "UPDATE dish SET name_dish=?, description=?, price=?, restaurant=?, type=? WHERE id_dish=?" ((Dish.name_dish menu), (Dish.description menu), (Dish.price menu), (Dish.restaurant menu), (Dish.type_dish menu), (Dish.id_dish menu))
    return result
