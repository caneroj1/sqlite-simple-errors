{-# LANGUAGE DeriveDataTypeable #-}

module Database.SQLite.SimpleErrors.Types where

import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Database.SQLite.Simple (FormatError, ResultError, SQLError)

-- | Constraint represents the kind of constraint violation returned by SQLite.
data Constraint = NotNull
                | ForeignKey
                | Unique
                | Check
  deriving (Show, Eq)

-- | SQLiteResponse is a wrapper around the different kinds of errors that can
-- be returned frm sqlite-simple. If there is a constraint error, then we will
-- construnct a SQLConstraintError instance.
data SQLiteResponse = SQLConstraintError Constraint Text
                    | SQLFormatError FormatError
                    | SQLResultError ResultError
                    | SQLOtherError  SQLError
  deriving (Show, Eq, Typeable)

instance Exception SQLiteResponse
