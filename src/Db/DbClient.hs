{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.DbClient where

import Domain
import qualified Entities.Client as Client
import qualified Entities.Dish as Dish
import qualified Entities.Restaurant as Restaurant
import qualified Entities.Delivery as Delivery
import qualified Domain as Domain
import qualified Entities.OrderRestaurant as OrderRestaurant
import qualified Database.PostgreSQL.Simple as D

insertClient conn client = do
    result <- D.execute conn "insert into user_restaurant (username,email,password,name,role,phone,identification,balance) values (?,?,?,?,?,?,?,?)" ((Client.username client),(Client.email client),(Client.password client),(Client.name client),(0 :: Int),(Client.phone client),(Client.identification client),(0::Int))
    return result

actualizarPassword conn client pass = do
    result <- D.execute conn "UPDATE user_restaurant  SET password = ? WHERE id_user = ?" (pass,(Client.id_user client))
    return result


updateClient conn client = do
  result <- D.execute conn "UPDATE user_restaurant SET email = ?, password = ?, phone = ?, identification = ? WHERE username = ?" ((Client.email client), (Client.password client), (Client.phone client), (Client.identification client), (Client.username client))
  return result

getAllClientes :: D.Connection -> IO [Client.Client]
getAllClientes conn = do
  list <- (D.query_ conn "select * from user_restaurant" :: IO [Client.Client])
  return list

getClient :: D.Connection -> Client.Client -> IO [Client.Client]
getClient conn client = do
    result <- (D.query conn "select * from user_restaurant where username = ? and password = ?" ((Client.username client), (Client.password client)) :: IO [Client.Client])
    return result

getClientByToken :: D.Connection -> Client.Client -> IO [Client.Client]
getClientByToken conn client = do
    result <- (D.query conn "select * from user_restaurant where token = ?" (D.Only (Client.token client)))
    return result

getClientByEmail :: D.Connection -> Client.Client -> IO [Client.Client]
getClientByEmail conn client = do
  result <- (D.query conn "select * from user_restaurant where email = ?" (D.Only (Client.email client)))
  return result

setToken conn client token=do
    result <- D.execute conn "UPDATE user_restaurant SET token=? WHERE username=?" (token,(Client.username client))
    return result

deleteToken conn client= do
  result <- D.execute conn "UPDATE user_restaurant SET token=NULL WHERE token=?" (D.Only (Client.token client))
  return result

getTokens :: D.Connection -> IO [Client.Client]
getTokens conn = do
  result <- (D.query_ conn "select * from user_restaurant where token is not null" :: IO [Client.Client])
  return result
