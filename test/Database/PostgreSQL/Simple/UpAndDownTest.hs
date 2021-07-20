module Database.PostgreSQL.Simple.UpAndDownTest where

import Database.PostgreSQL.Simple.UpAndDown
import Text.Megaparsec
import qualified Data.Text.IO as T
import Test.Hspec (Spec, describe, it, fit)
import Test.Hspec.Megaparsec

upAndDownSpec :: Spec
upAndDownSpec = describe "Testing up and down migration files" $ do
  fit "Parse Up  label" $ do
    let parseContent = runParser (parseUpLabel <* eof) "<test>"
    parseContent "-- !Ups\n" `shouldParse` Up
  fit "Parse Down  label" $ do
    let parseContent = runParser (parseDownLabel <* eof) "<test>"
    parseContent "-- !Downs\n" `shouldParse` Down
  fit "Parse a migration file" $ do
    let parseContent = runParser (parseMigration <* eof) "./share/test/up_down.sql"
    file <- T.readFile "./share/test/up_down.sql"
    parseContent file `shouldParse` []
