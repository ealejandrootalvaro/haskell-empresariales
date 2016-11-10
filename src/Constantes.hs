{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Constantes where

import           Network.HaskellNet.IMAP.SSL
import           Network.HaskellNet.SMTP.SSL as SMTP

import           Network.HaskellNet.Auth (AuthType(LOGIN))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as L



success :: String
success = "Success"

error' :: String
error' = "Error"

connectionStr :: B.ByteString
-- Conexion a la base de datos de pago
connectionStr = "postgresql://postgres:94cbd72b4e4133f3417a61adf9a418b1@138.197.15.163:5454/restaurant"
puerto :: Int
puerto = 8087



username = "restappranteinfo@gmail.com"
password = "pekkieressaberlo"




smtpTest recipient newPassword = doSMTPSTARTTLS "smtp.gmail.com" $ \c -> do
    authSucceed <- SMTP.authenticate LOGIN username password c
    if authSucceed
      then sendPlainTextMail recipient username subject body c
      else print "Authentication error."
  where subject = "Recuperar contraseña"
        body    = L.pack("Su nueva contraseña es "++newPassword)

sendMensaje :: String -> String -> IO ()
sendMensaje recipient newPassword = (smtpTest recipient newPassword) >> return ()
