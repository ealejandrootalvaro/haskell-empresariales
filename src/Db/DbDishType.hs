{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.DbDishType where

import Domain
import qualified Entities.Client as Client
import qualified Entities.Dish as Dish
import qualified Entities.Restaurant as Restaurant
import qualified Entities.Delivery as Delivery
import qualified Domain as Domain
import qualified Entities.OrderRestaurant as OrderRestaurant
import qualified Database.PostgreSQL.Simple as D

getAllDishType :: D.Connection -> IO [Dish.Dish_type]
getAllDishType c  = do
    list <- (D.query_ c "select * from dish_type" :: IO [Dish.Dish_type])
    return list
