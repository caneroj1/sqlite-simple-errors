{-# LANGUAGE OverloadedStrings #-}

module SQLUtils where

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Text (Text)

createTableSQL :: Query
createTableSQL =
  "CREATE TABLE Test (name varchar(50) NOT NULL UNIQUE\
  \                  ,age  int NOT NULL CHECK(age > 0))"

insertSQL :: Query
insertSQL =
  "INSERT INTO Test (name, age) VALUES (?, ?)"
