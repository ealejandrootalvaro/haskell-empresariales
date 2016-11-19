{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, ViewPatterns #-}

module Main where

import Db.DbClient
import Db.DbDish
import Db.DbDishType
import Db.DbRestaurant
import Db.DbDelivery
import Utilities.Constants
import Utilities.UtilitiesFunctions
import Domain
import Web.Scotty
import Data.Maybe
import Control.Monad
import System.Random
import Control.Exception
import System.Environment
import Control.Monad.IO.Class
import Network.HTTP.Types.Status
import Database.PostgreSQL.Simple.URL
import Database.PostgreSQL.Simple.Errors
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Text as T
import qualified Data.Text.Lazy as A
import qualified Data.ByteString.Char8 as B
import qualified Database.PostgreSQL.Simple as D
import qualified Network.Wai.Middleware.Cors as C

import qualified Entities.Client as Client
import qualified Entities.Dish as Dish
import qualified Entities.Restaurant as Restaurant
import qualified Entities.OrderRestaurant as OrderRestaurant
import qualified Entities.Delivery as Delivery


main = do

  putStrLn "Starting Server..."
  conn <- D.connectPostgreSQL connectionStr
  env <- getEnvironment
  let port = maybe puerto read $ lookup "PORT" env
  scotty port $ do
    middleware C.simpleCors

    get "/" $ do
      text ("Bienvenido servicios REST construidos con Haskell")

--------------------------------------MENU--------------------------------------
    get "/menus" $ do
      variable <- liftIO (getAllMenus conn)
      json variable


    get "/menus/:id" $ do
      asd <- param "id" :: ActionM Integer
      menu <- liftIO $ getMenuById conn asd
      case menu of
        ([]) ->  json (Resultado {tipo= Just error', mensaje= Just "Menu no encontrado"})
        x -> json menu


    post "/menus" $ do
      menu <- (jsonData :: ActionM Dish.Dish)
      case (validarMenu menu) of
        []->do
          response <- liftIO $ try $ insertMenu conn menu
          case response of
            Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Menu agregado"}) >> status created201
            Left e -> json (sqlError e)
        xs->do
          json (Resultado {tipo= Just error', mensaje= Just ("Campos invalidos: " ++ (concatListString xs))})


    put "/actualizarMenu" $ do
      menu <- (jsonData :: ActionM Dish.Dish)
      response <- liftIO $ try $ updateMenu conn menu
      case response of
        Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Menu actualizado"}) >> status created201
        Left e -> json (sqlError e)

---------------------------------Tipo MENU--------------------------------------

    get "/tipoMenu" $ do
      variable <- liftIO (getAllDishType conn)
      json variable

--------------------------------RESTAURANT--------------------------------------

    get "/restaurantes" $ do
      variable <- liftIO (getAllRestaurants conn)
      json variable

---------------------------------CLIENTE----------------------------------------
    post "/clientes" $ do
      client <- (jsonData :: ActionM Client.Client)
      case (validarCliente client) of
        []->do
          response <- liftIO $ try $ insertClient conn client
          case response of
            Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Cliente agregado"}) >> status created201
            Left e -> json (sqlError e)
        xs->do
          json (Resultado {tipo= Just error', mensaje= Just ("Campos invalidos: " ++ (concatListString xs))})


    get "/clientes" $ do
      variable <- liftIO (getAllClientes conn)
      json variable

    put "/clientes" $ do
      client <- (jsonData :: ActionM Client.Client)
      case (validarCliente client) of
        []->do
          response <- liftIO $ try $ updateClient conn client
          case response of
            Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Informacion modificada"}) >> status created201
            Left e -> json (sqlError e)
        xs->do
          json (Resultado {tipo= Just error', mensaje= Just ("Campos invalidos: " ++ (concatListString xs))})


    post "/recuperarPassword" $ do
      client <- (jsonData :: ActionM Client.Client)
      resp <- liftIO $ getClientByEmail conn client
      case resp of
        [] -> json (Resultado {tipo= Just error', mensaje= Just "Usuario no existe"})
        (x:[]) -> do
          let contr = take 8 $ randomString 1
          up <- liftIO $ actualizarPassword conn x contr
          res <- liftIO $ sendMensaje (fromJust (Client.email x)) contr
          json (Resultado {tipo= Just success, mensaje= Just "Nueva contraseña enviada al correo"})


    put "/iniciarSesion" $ do
      client <- (jsonData :: ActionM Client.Client)
      resp <- liftIO $ getClient conn client
      case resp of
          [] ->json (Resultado {tipo= Just error', mensaje= Just "Usuario o contraseña incorrecto"})
          _ ->  do
            tokens <- liftIO $ getTokens conn
            let token= tokenGenerator 2 (randomString 1) (filterToken tokens)
            response <- liftIO $ try $ setToken conn client (token :: String)
            case response of
              Right _ -> json (Resultado {tipo= Just success, mensaje= Just token}) >> status created201
              Left e -> json (sqlError e)


    put "/cerrarSesion" $ do
      client <- (jsonData :: ActionM Client.Client)
      resp <- liftIO $ getClientByToken conn client
      case resp of
        []->json (Resultado {tipo= Just error', mensaje= Just "Token invalido"})
        _ -> do
          response <- liftIO $ try $ deleteToken conn client
          case response of
            Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Sesion cerrada"}) >> status created201
            Left e -> json (sqlError e)
------------------------------------DOMICILIOS----------------------------------
    post "/domicilio" $ do
        delivery <- (jsonData :: ActionM Delivery.Delivery)
        let orders=(Delivery.orders delivery)
        id_delivery <- liftIO $ insertDelivery conn delivery
        case id_delivery of
        []-> json (Resultado {tipo= Just error', mensaje = Just "Error creando el domicilio"})
        _ -> do
          let delivery= (getInt $ head id_delivery)
          total <- saveOrders conn delivery orders 0
          json (ResultadoDomiclio {tipo= Just success, total= Just (show total)}) >> status created201
