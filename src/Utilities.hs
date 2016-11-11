{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Utilities where
import Domain
import System.Random
import Text.Regex.Posix
import Data.Maybe
import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.SMTP.SSL as SMTP
import Network.HaskellNet.Auth (AuthType(LOGIN))
import qualified Data.Text.Lazy as L
import qualified Constantes as C
import qualified Database.PostgreSQL.Simple as D
import qualified Data.ByteString.Char8 as B
import qualified Entities.Client as Client
import qualified Entities.Dish as Dish
-------------------------------aleatoriedad-------------------------------------
randomString :: Int -> String
randomString gen=take 30 $ randomRs ('a','z') (mkStdGen gen) :: String

tokenGenerator :: Int -> String -> [String] -> String
tokenGenerator gen t tokens=
  if (length listaTokens)>0
    then
      tokenGenerator (gen+1) (randomString gen) tokens
    else t
  where
    listaTokens = filter (\x-> x==t) tokens

-----------------------------validaciones---------------------------------------
patternNum="^([0-9]+)$"
patternFloat="^([0-9]+[.])?[0-9]+$"
patternEmail="[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}"

regexMatch :: String -> String -> Bool
regexMatch str expr= str =~ expr :: (Bool)

validarMenu menu=
  filter (\x -> (length x)>0) (validarPrecio:[])
  where
    validarPrecio=
      if regexMatch (show $ fromJust $ Dish.price menu) patternFloat
        then ""
        else "price"

validarCliente client=
    filter (\x -> (length x)>0) (validarTelefono:validarEmail:validarIdetificacion:[])
    where
      validarIdetificacion=
        if isJust (Client.identification client)
          then
            if regexMatch (fromJust $ Client.identification client) patternNum
              then ""
              else "identification"
          else ""
      validarTelefono=
        if isJust (Client.phone client)
          then
            if regexMatch (fromJust (Client.phone client)) patternNum
              then ""
              else "phone"
          else ""
      validarEmail=
          if regexMatch (fromJust (Client.email client)) patternEmail
            then ""
            else "email"

---------------------------------email------------------------------------------
smtpTest recipient newPassword = doSMTPSTARTTLS "smtp.gmail.com" $ \c -> do
    authSucceed <- SMTP.authenticate LOGIN C.username C.password c
    if authSucceed
      then sendPlainTextMail recipient C.username subject body c
      else print "Authentication error."
  where subject = "Recuperar contraseña"
        body    = L.pack("Su nueva contraseña es "++newPassword)

sendMensaje :: String -> String -> IO ()
sendMensaje recipient newPassword = (smtpTest recipient newPassword) >> return ()
---------------------------------otros------------------------------------------

sqlError :: D.SqlError -> Resultado
sqlError err=
  case (D.sqlState err) of
    "23505"->(ResultadoConCampos {tipo= Just C.error',
                               mensaje= Just (B.unpack $ D.sqlErrorDetail err),
                               campos= Just (extractFieldSqlError((B.unpack $ D.sqlErrorDetail err)):[])})
    _->(Resultado {tipo= Just C.error', mensaje= Just (B.unpack $ D.sqlErrorDetail err)})

extractFieldSqlError :: String -> String
extractFieldSqlError (x:xs)
  |x=='(' = parentesis [] xs
  |otherwise= extractFieldSqlError xs
  where
    parentesis err (y:ys)
        |y==')' = err
        |otherwise = parentesis (err++[y]) ys


concatListString :: [String]-> String
concatListString []=[]
concatListString (x:xs)
  |xs==[]= x++[]
  |otherwise= x ++ ", " ++concatListString xs

filterToken :: [Client.Client]->[String]
filterToken []=[]
filterToken (x:xs)= (fromJust(Client.token x)):filterToken xs
