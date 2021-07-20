{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.PostgreSQL.Simple.UpAndDown where

import Control.Monad.Combinators
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec 
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text

data Label = Up | Down
  deriving stock (Eq, Show)

data Section = Section Label ByteString
  deriving stock (Eq, Show)

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol' spaceConsumer

semicolon :: Parser Text
semicolon = symbol ";"

parseUpLabel :: Parser Label
parseUpLabel = do
  string "-- !Ups"
  eol
  pure Up

parseUpCode :: Parser ByteString
parseUpCode = do
  result ::  [[String]]<- dbg "up code" $ many $ manyTill (many L.charLiteral) semicolon
  pure . pack . mconcat . mconcat $ result

parseDownLabel :: Parser Label
parseDownLabel = do
  string "-- !Downs"
  eol
  pure Down

parseDownCode :: Parser ByteString
parseDownCode = do
  result <- dbg "down code" $ many $ manyTill L.charLiteral (char ';' <|> (eof >> pure ' '))
  pure . pack . unlines $ result

parseMigration :: Parser [Section]
parseMigration = do
  upLabel <- parseUpLabel
  upCode  <- parseUpCode
  downLabel <- parseDownLabel
  downCode <- parseDownCode
  pure [Section upLabel upCode, Section downLabel downCode]
