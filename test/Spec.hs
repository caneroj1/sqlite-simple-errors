{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.SimpleErrors
import Database.SQLite.SimpleErrors.Types
import SQLUtils
import Utils

main :: IO ()
main = executeTestRunner [checkNotNullConstraintName,
                          checkNotNullConstraintAge,
                          checkUniqueConstraint,
                          checkCheckConstraint,
                          checkForeignKeyConstraint]

checkNotNullConstraintName :: Test (DatabaseResponse ())
checkNotNullConstraintName = Test "Not Null Constraint - Name"
                                  (Left $ SQLConstraintError NotNull "Test.name")
                                  test
  where test conn = runDBAction $ execute conn insertSQL sqlData
        sqlData :: (Maybe Text, Int)
        sqlData = (Nothing, 10)

checkNotNullConstraintAge :: Test (DatabaseResponse ())
checkNotNullConstraintAge = Test "Not Null Constraint - Age"
                                 (Left $ SQLConstraintError NotNull "Test.age")
                                 test
  where test conn = runDBAction $ execute conn insertSQL sqlData
        sqlData :: (Text, Maybe Int)
        sqlData = ("name", Nothing)

checkUniqueConstraint :: Test (DatabaseResponse ())
checkUniqueConstraint = Test "Unique Constraint - Name"
                             (Left $ SQLConstraintError Unique "Test.name")
                             test
  where
    test conn =
      runDBAction (execute conn insertSQL sqlData) >>
        runDBAction (execute conn insertSQL sqlData)
    sqlData :: (Text, Int)
    sqlData = ("name", 10)

checkCheckConstraint :: Test (DatabaseResponse ())
checkCheckConstraint = Test "Check Constraint - Age"
                            (Left $ SQLConstraintError Check "Test")
                            test
  where
    test conn = runDBAction $ execute conn insertSQL sqlData
    sqlData :: (Text, Int)
    sqlData = ("name", 0)

checkForeignKeyConstraint :: Test (DatabaseResponse ())
checkForeignKeyConstraint = Test "Check Foreign Key Constraint - TestID"
                                 (Left $ SQLConstraintError ForeignKey "")
                                 test
  where
    test conn = runDBAction $ execute conn insertOtherSQL sqlData
    sqlData :: (Text, Int)
    sqlData = ("other", 10)
