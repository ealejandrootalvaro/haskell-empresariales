{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Constantes where
import Data.ByteString.Char8

success :: String
success = "Success"

error' :: String
error' = "Error"

connectionStr :: ByteString
--connectionStr = "postgres://xlyrtaxdwdozqh:ml6B7YXEWvGLdpcw5ty-BDrcne@ec2-50-19-240-113.compute-1.amazonaws.com:5432/dcku066iig1lq1"
connectionStr = "postgresql://postgres:94cbd72b4e4133f3417a61adf9a418b1@138.197.15.163:5454/restaurant"
puerto :: Int
puerto = 8087
