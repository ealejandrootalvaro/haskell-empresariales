{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.DbRestaurant where

import Domain
import qualified Entities.Client as Client
import qualified Entities.Dish as Dish
import qualified Entities.Restaurant as Restaurant
import qualified Entities.Delivery as Delivery
import qualified Domain as Domain
import qualified Entities.OrderRestaurant as OrderRestaurant
import qualified Database.PostgreSQL.Simple as D

getAllRestaurants :: D.Connection -> IO [Restaurant.Restaurant]
getAllRestaurants c  = do
    list <- (D.query_ c "select * from restaurant" :: IO [Restaurant.Restaurant])
    return list
