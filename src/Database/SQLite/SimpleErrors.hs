module Database.SQLite.SimpleErrors
(
  DatabaseResponse
, runDBAction
) where

import Control.Exception
import Database.SQLite.Simple (FormatError, ResultError, SQLError)
import Database.SQLite.SimpleErrors.Types
import Database.SQLite.SimpleErrors.Parser

type DatabaseResponse a = Either SQLiteResponse a

runDBAction :: IO a -> IO (DatabaseResponse a)
runDBAction sqlAction = do
  res <- try sqlAction
  case res of
    (Left e)  -> return . Left $ convertException e
    (Right e) -> return $ Right e

convertException :: SomeException -> SQLiteResponse
convertException se = handleResultError se $ fromException se

handleResultError :: SomeException -> Maybe ResultError -> SQLiteResponse
handleResultError se Nothing = handleFormatError se (fromException se)
handleResultError _ (Just e) = SQLResultError e

handleFormatError :: SomeException -> Maybe FormatError -> SQLiteResponse
handleFormatError se Nothing = handleSQLError se (fromException se)
handleFormatError _ (Just e) = SQLFormatError e

handleSQLError :: SomeException -> Maybe SQLError -> SQLiteResponse
handleSQLError se Nothing = throw se
handleSQLError _ (Just e) = receiveSQLError e
