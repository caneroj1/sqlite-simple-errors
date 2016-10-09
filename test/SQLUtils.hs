{-# LANGUAGE OverloadedStrings #-}

module SQLUtils where

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Text (Text)

createTableSQL :: Query
createTableSQL =
  "CREATE TABLE if not exists Test (name varchar(50) NOT NULL UNIQUE\
  \                  ,age  int NOT NULL CHECK(age > 0)              \
  \                  ,id   integer PRIMARY KEY NOT NULL);          "

createNextTableSQL :: Query
createNextTableSQL =
  "CREATE TABLE if not exists Next (label varchar(10)               \
  \                  ,test_id int NOT NULL                          \
  \                  ,FOREIGN KEY(test_id) REFERENCES Test(id));"

insertSQL :: Query
insertSQL =
  "INSERT INTO Test (name, age) VALUES (?, ?)"

insertOtherSQL :: Query
insertOtherSQL =
  "INSERT INTO Next (label, test_id) VALUES (?, ?)"
