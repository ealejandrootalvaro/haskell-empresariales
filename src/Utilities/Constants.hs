{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Utilities.Constants where
import qualified Data.ByteString.Char8 as B

success :: String
success = "Success"

error' :: String
error' = "Error"

connectionStr :: B.ByteString
-- Conexion a la base de datos de pago
connectionStr = "postgresql://postgres:94cbd72b4e4133f3417a61adf9a418b1@138.197.15.163:5454/restaurant"

puerto :: Int
puerto = 8087

username :: String
username = "restappranteinfo@gmail.com"

password :: String
password = "pakkieressaberlo"
