-- |
-- Module      : Main
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A standalone program for the postgresql-simple-migration library.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Exception
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.PostgreSQL.Simple ( SqlError (..)
                                            , connectPostgreSQL
                                            , withTransaction
                                            )
import           Database.PostgreSQL.Simple.Migration ( MigrationCommand (..)
                                                      , MigrationOptions (..)
                                                      , MigrationResult (..)
                                                      , Verbosity (..)
                                                      , defaultOptions
                                                      , runMigration
                                                      )
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (Handle, hPutStrLn, stdout, stderr)


main :: IO ()
main = getArgs >>= \case
  "-h":_ ->
    printUsage stdout
  "-q":xs ->
    ppException $ run (parseCommand xs) False
  xs ->
    ppException $ run (parseCommand xs) True


-- | Pretty print postgresql-simple exceptions to see whats going on
ppException :: IO a -> IO a
ppException a = catch a ehandler
  where
    ehandler e = maybe (throw e) (*> exitFailure) (pSqlError <$> fromException e)
    bsToString = T.unpack . T.decodeUtf8
    pSqlError e = mapM_ (hPutStrLn stderr)
                  [ "SqlError:"
                  , "  sqlState: ", bsToString $ sqlState e
                  , "  sqlExecStatus: ", show $ sqlExecStatus e
                  , "  sqlErrorMsg: ", bsToString $ sqlErrorMsg e
                  , "  sqlErrorDetail: ", bsToString $ sqlErrorDetail e
                  , "  sqlErrorHint: ", bsToString $ sqlErrorHint e
                  ]

run
  :: Maybe Command
  -> Bool
  -> IO ()
run Nothing _ = printUsage stderr >> exitFailure
run (Just cmd) verbose =
  handleResult =<< case cmd of
    Initialize url tableName -> do
      con <- connectPostgreSQL (BS8.pack url)
      let opts = defaultOptions
           { optTableName = tableName
           , optVerbose = if verbose then Verbose else Quiet
           }
      withTransaction con $ runMigration con opts MigrationInitialization

    Migrate url dir tableName -> do
      con <- connectPostgreSQL (BS8.pack url)
      let opts = defaultOptions
           { optTableName = tableName
           , optVerbose = if verbose then Verbose else Quiet
           }
      withTransaction con $ runMigration con opts $ MigrationDirectory dir

    Validate url dir tableName -> do
      con <- connectPostgreSQL $ BS8.pack url
      let opts = defaultOptions
           { optTableName = tableName
           , optVerbose = if verbose then Verbose else Quiet
           }
      withTransaction con $ runMigration con opts $ MigrationValidation (MigrationDirectory dir)

  where
    handleResult MigrationSuccess = exitSuccess
    handleResult (MigrationError _) = exitFailure


parseCommand :: [String] -> Maybe Command
parseCommand ("init":url:tableName:_) = Just (Initialize url (BS8.pack tableName))
parseCommand ("migrate":url:dir:tableName:_) = Just (Migrate url dir (BS8.pack tableName))
parseCommand ("validate":url:dir:tableName:_) = Just (Validate url dir (BS8.pack tableName))
parseCommand ("init":url:_) = Just (Initialize url "schema_migrations")
parseCommand ("migrate":url:dir:_) = Just (Migrate url dir "schema_migrations")
parseCommand ("validate":url:dir:_) = Just (Validate url dir "schema_migrations")
parseCommand _ = Nothing


printUsage :: Handle -> IO ()
printUsage h = do
    hPutStrLn h "migrate [options] <command>"
    hPutStrLn h "  Options:"
    hPutStrLn h "      -h          Print help text"
    hPutStrLn h "      -q          Enable quiet mode"
    hPutStrLn h "  Commands:"
    hPutStrLn h "      init <con> {migrations table name}"
    hPutStrLn h "                  Initialize the database. Required to be run"
    hPutStrLn h "                  at least once."
    hPutStrLn h "                  {migrations table name} is the optiona name."
    hPutStrLn h "                  for the migrations table. This defaults to"
    hPutStrLn h "                  `schema_migrations`."
    hPutStrLn h "      migrate <con> <directory> {migrations table name}"
    hPutStrLn h "                  Execute all SQL scripts in the provided"
    hPutStrLn h "                  directory in alphabetical order."
    hPutStrLn h "                  Scripts that have already been executed are"
    hPutStrLn h "                  ignored. If a script was changed since the"
    hPutStrLn h "                  time of its last execution, an error is"
    hPutStrLn h "                  raised."
    hPutStrLn h "                  {migrations table name} is the optiona name."
    hPutStrLn h "                  for the migrations table. This defaults to"
    hPutStrLn h "                  `schema_migrations`."
    hPutStrLn h "      validate <con> <directory> {migrations table name}"
    hPutStrLn h "                  Validate all SQL scripts in the provided"
    hPutStrLn h "                  directory."
    hPutStrLn h "                  {migrations table name} is the optiona name."
    hPutStrLn h "                  for the migrations table. This defaults to"
    hPutStrLn h "                  `schema_migrations`."
    hPutStrLn h "      The <con> parameter is based on libpq connection string"
    hPutStrLn h "      syntax. Detailled information is available here:"
    hPutStrLn h "      <http://www.postgresql.org/docs/9.3/static/libpq-connect.html>"


data Command
  = Initialize String BS.ByteString
  | Migrate String FilePath BS.ByteString
  | Validate String FilePath BS.ByteString
