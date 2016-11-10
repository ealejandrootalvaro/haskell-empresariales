  {-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, ViewPatterns #-}

module Main where


import Db
import Domain
import Constantes
import Utilities
--import Data.Monoid ((<>))
--import Data.Aeson (FromJSON, ToJSON)

import Web.Scotty
import Control.Monad
import Data.Maybe
import Database.PostgreSQL.Simple.URL
import Control.Monad.IO.Class
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Types.Status
import System.Environment
import Control.Exception
import Database.PostgreSQL.Simple.Errors
import System.Random
import qualified Data.ByteString.Char8 as B
import qualified Database.PostgreSQL.Simple as D
import qualified Network.Wai.Middleware.Cors as C

import qualified Data.Text as T
import qualified Data.Text.Lazy as A

main = do

  putStrLn "Starting Server..."
  conn <- D.connectPostgreSQL connectionStr
  env <- getEnvironment
  let port = maybe puerto read $ lookup "PORT" env
  scotty port $ do
    middleware C.simpleCors

    get "/" $ do
      text ("Bienvenido a un servicio REST construido con Haskell")

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
      menu <- (jsonData :: ActionM Dish)
      response <- liftIO $ try $ insertMenu conn menu
      case response of
        Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Menu agregado"}) >> status created201
        Left e -> json (Resultado {tipo= Just error', mensaje= Just (B.unpack $ D.sqlErrorMsg e)})

--------------------------------------Tipo MENU--------------------------------------

    get "/tipoMenu" $ do
      variable <- liftIO (getAllDishType conn)
      json variable


------------------------------- RESTAURANT-------------------------------

    get "/restaurantes" $ do
      variable <- liftIO (getAllRestaurants conn)
      json variable


--------------------------------------CLIENTE-----------------------------------
    post "/clientes" $ do
      client <- (jsonData :: ActionM Client)
      case (validarCliente client) of
        []->do
          response <- liftIO $ try $ insertClient conn client
          case response of
            Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Cliente agregado"}) >> status created201
            Left e -> json (Resultado {tipo= Just error', mensaje= Just (B.unpack $ D.sqlErrorMsg e)})
        xs->do
          json (Resultado {tipo= Just error', mensaje= Just ("Campos invalidos: " ++ (concatListString xs))})


    get "/clientes" $ do
      variable <- liftIO (getAllClientes conn)
      json variable
      
      
    post "/recuperarPassword" $ do
      client <- (jsonData :: ActionM Client)
      resp <- liftIO $ getClientByEmail conn client
      
      case resp of
        [] -> json (Resultado {tipo= Just error', mensaje= Just "Usuario no existe"})
        (x:[]) -> do
          let contr = take 8 $ randomString 1
          res <- liftIO $ sendMensaje (fromJust (email x)) contr 
          json (Resultado {tipo= Just success, mensaje= Just "Nueva contraseña enviada al correo"})



    put "/iniciarSesion" $ do
      client <- (jsonData :: ActionM Client)
      resp <- liftIO $ getClient conn client
      case resp of
          [] ->json (Resultado {tipo= Just error', mensaje= Just "Usuario o contraseña incorrecto"})
          _ ->  do
            tokens <- liftIO $ getTokens conn
            let token= tokenGenerator 2 (randomString 1) (filterToken tokens)
            response <- liftIO $ try $ setToken conn client (token :: String)
            case response of
              Right _ -> json (Resultado {tipo= Just success, mensaje= Just token}) >> status created201
              Left e -> json (Resultado {tipo= Just error', mensaje= Just (B.unpack $ D.sqlErrorMsg e)})

    put "/cerrarSesion" $ do
      client <- (jsonData :: ActionM Client)
      resp <- liftIO $ getClientByToken conn client
      case resp of
        []->json (Resultado {tipo= Just error', mensaje= Just "Token invalido"})
        _ -> do
          response <- liftIO $ try $ deleteToken conn client
          case response of
            Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Sesion cerrada"}) >> status created201
            Left e -> json (Resultado {tipo= Just error', mensaje= Just (B.unpack $ D.sqlErrorMsg e)})


filterToken :: [Client]->[String]
filterToken []=[]
filterToken (x:xs)= (fromJust(token x)):filterToken xs
