module Database.SQLite.SimpleErrors.Parser where

import Control.Monad
import           Data.Text                (Text)
import qualified Data.Text as Text hiding (Text)
import Database.SQLite.Simple
import Database.SQLite.SimpleErrors.Types
import Text.Parsec
import Text.Parsec.Text
import Debug.Trace

type SQLiteParser = Parser SQLiteResponse

constraintName :: String -> Parser ()
constraintName n = void $ string (n ++ " constraint failed:") >> spaces

getRest :: Parser Text
getRest = Text.pack <$> many1 anyChar

parseConstraint :: String -> Constraint -> SQLiteParser
parseConstraint n c = constraintName n >> SQLConstraintError c <$> getRest

constraintParser :: Parsec Text () SQLiteResponse
constraintParser =
  parseConstraint "PRIMARY KEY" PrimaryKey <|>
  parseConstraint "NOT NULL"    NotNull    <|>
  parseConstraint "DEFAULT"     Default    <|>
  parseConstraint "UNIQUE"      Unique     <|>
  parseConstraint "CHECK"       Check

parseError :: SQLError -> SQLiteResponse
parseError e@SQLError{sqlErrorDetails = details} =
  either (\_ -> SQLOtherError e) id $ parse constraintParser "" (trace (show details) details)

receiveSQLError :: SQLError -> SQLiteResponse
receiveSQLError e@SQLError{sqlError = ErrorConstraint} = parseError e
receiveSQLError e = SQLOtherError e
