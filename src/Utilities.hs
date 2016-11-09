{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Utilities where
import Domain
import System.Random
import Text.Regex.Posix
import Data.Maybe
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
patternNum="([0-9]+)"
patternEmail="[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}"

regexMatch :: String -> String -> Bool
regexMatch str expr= str =~ expr :: (Bool)

validarCliente client=
    filter (\x -> (length x)>0) (validarTelefono:validarEmail:validarIdetificacion:[])
    where
      validarIdetificacion=
        if isJust (identification client)
          then
            if regexMatch (fromJust (identification client)) patternNum
              then ""
              else "identification"
          else ""
      validarTelefono=
        if isJust (phone client)
          then
            if regexMatch (fromJust (phone client)) patternNum
              then ""
              else "phone"
          else ""
      validarEmail=
          if regexMatch (fromJust (email client)) patternEmail
            then ""
            else "email"
---------------------------------otros------------------------------------------
concatListString :: [String]-> String
concatListString []=[]
concatListString (x:xs)
  |xs==[]= x++[]
  |otherwise= x ++ ", " ++concatListString xs

