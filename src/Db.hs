{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where

import Domain
import qualified Database.PostgreSQL.Simple as D

-----------------------------------MENU-----------------------------------------
getAllMenus :: D.Connection -> IO [Dish]
getAllMenus c = do
  list <- (D.query_ c "select * from dish" :: IO [Dish])
  return list


getMenuById :: D.Connection -> Integer -> IO [Dish]
getMenuById conn int = do
    menu <- (D.query conn "select * from dish where id_dish = ?" (D.Only int) :: IO [Dish])
    return menu

insertMenu conn menu = do
    result <- D.execute conn "insert into dish (name_dish,description,price,restaurant,type) values (?,?,?,?,?)" ((name_dish menu), (description menu), (price menu),(restaurant menu),(type_dish menu))
    return result

-----------------------------------Tipo MENU-----------------------------------------
getAllDishType :: D.Connection -> IO [Dish_type]
getAllDishType c  = do
    list <- (D.query_ c "select * from dish_type" :: IO [Dish_type])
    return list

------------------------------- RESTAURANT-------------------------------

getAllRestaurants :: D.Connection -> IO [Restaurant]
getAllRestaurants c  = do
    list <- (D.query_ c "select * from restaurant" :: IO [Restaurant])
    return list


------------------------------- CLIENT_RESTAURANT-------------------------------
insertClient conn client = do
    result <- D.execute conn "insert into user_restaurant (username,email,password,name,role,phone,identification) values (?,?,?,?,?,?,?)" ((username client),(email client),(password client),(nameUser client),(0 :: Int),(phone client),(identification client))
    return result

getAllClientes :: D.Connection -> IO [Client]
getAllClientes conn = do
  list <- (D.query_ conn "select * from user_restaurant" :: IO [Client])
  return list

getClient :: D.Connection -> Client -> IO [Client]
getClient conn client = do
    result <- (D.query conn "select * from user_restaurant where username = ? and password = ?" ((username client), (password client)) :: IO [Client])
    return result

getClientByToken :: D.Connection -> Client -> IO [Client]
getClientByToken conn client = do
    result <- (D.query conn "select * from user_restaurant where token = ?" (D.Only (token client)))
    return result

setToken conn client token=do
    result <- D.execute conn "UPDATE user_restaurant SET token=? WHERE username=?" (token,(username client))
    return result

deleteToken conn client= do
  result <- D.execute conn "UPDATE user_restaurant SET token=NULL WHERE token=?" (D.Only (token client))
  return result

getTokens :: D.Connection -> IO [Client]
getTokens conn = do
  result <- (D.query_ conn "select * from user_restaurant where token is not null" :: IO [Client])
  return result
