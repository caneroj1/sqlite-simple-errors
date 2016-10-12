{-|
Module      : Database.SQLite.SimpleErrors.Parser
Description : Parsing logic
Copyright   : (c) Joseph Canero, 2016
License     : BSD-3
Maintainer  : jmc41493@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Database.SQLite.SimpleErrors.Parser
(
  receiveSQLError
)
where

import Control.Applicative ((<$>))
import Control.Monad
import           Data.Text                (Text)
import qualified Data.Text as Text hiding (Text)
import Database.SQLite.Simple
import Database.SQLite.SimpleErrors.Types
import Text.Parsec
import Text.Parsec.Text

type SQLiteParser = Parser SQLiteResponse

constraintNameOnly :: String -> Parser ()
constraintNameOnly n = void $ string (n ++ " constraint failed")

constraintName :: String -> Parser ()
constraintName n = constraintNameOnly n >> char ':' >> spaces

getRest :: Parser Text
getRest = Text.pack <$> many1 anyChar

parseConstraint :: String -> Constraint -> SQLiteParser
parseConstraint n c = constraintName n >> SQLConstraintError c <$> getRest

parseConstraintNoDetails :: String -> Constraint -> SQLiteParser
parseConstraintNoDetails n c =
  constraintNameOnly n >> return (SQLConstraintError c Text.empty)

constraintParser :: Parsec Text () SQLiteResponse
constraintParser =
  parseConstraintNoDetails "FOREIGN KEY" ForeignKey <|>
  parseConstraint "NOT NULL"    NotNull             <|>
  parseConstraint "UNIQUE"      Unique              <|>
  parseConstraint "CHECK"       Check

parseError :: SQLError -> SQLiteResponse
parseError e@SQLError{sqlErrorDetails = details} =
  either (\_ -> SQLOtherError e) id $ parse constraintParser "" details

-- | Given a SQL error, converts it into a SQLiteResponse.
-- If the error is not an ErrorConstraint, it is essentially just wrapped in
-- SQLOtherError.
-- If the error is an ErrorConstraint error, try to parse the error as one
-- of the following kinds of constraint violations: Foreign Key, Not Null,
-- Unique, or Check.
receiveSQLError :: SQLError -> SQLiteResponse
receiveSQLError e@SQLError{sqlError = ErrorConstraint} = parseError e
receiveSQLError e = SQLOtherError e
