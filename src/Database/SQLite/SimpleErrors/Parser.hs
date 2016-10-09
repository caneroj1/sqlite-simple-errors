module Database.SQLite.SimpleErrors.Parser where

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

receiveSQLError :: SQLError -> SQLiteResponse
receiveSQLError e@SQLError{sqlError = ErrorConstraint} = parseError e
receiveSQLError e = SQLOtherError e
