{-# LANGUAGE DeriveDataTypeable #-}

module Database.SQLite.SimpleErrors.Types where

import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Database.SQLite.Simple (FormatError, ResultError, SQLError)

data Constraint = NotNull
                | ForeignKey
                | Unique
                | Check
  deriving (Show, Eq)

data SQLiteResponse = SQLConstraintError Constraint Text
                    | SQLFormatError FormatError
                    | SQLResultError ResultError
                    | SQLOtherError  SQLError
  deriving (Show, Eq, Typeable)

instance Exception SQLiteResponse
