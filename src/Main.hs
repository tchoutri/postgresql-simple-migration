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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Exception
import qualified Data.ByteString                      as BS (ByteString)
import qualified Data.ByteString.Char8                as BS8 (pack)
import           Database.PostgreSQL.Simple           (SqlError (..),
                                                       connectPostgreSQL,
                                                       withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext' (..),
                                                       MigrationResult (..),
                                                       runMigration')
import           System.Environment                   (getArgs)
import           System.Exit                          (exitFailure, exitSuccess)
import           System.IO                            (Handle, hPutStrLn,
                                                       stdout, stderr)

import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T

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
    ehandler e = maybe (throw e) (*> exitFailure)
                 (pSqlError <$> fromException e)
    bsToString = T.unpack . T.decodeUtf8
    pSqlError e = mapM_ (hPutStrLn stderr)
                  [ "SqlError:"
                  , "  sqlState: "
                  , bsToString $ sqlState e
                  , "  sqlExecStatus: "
                  , show $ sqlExecStatus e
                  , "  sqlErrorMsg: "
                  , bsToString $ sqlErrorMsg e
                  , "  sqlErrorDetail: "
                  , bsToString $ sqlErrorDetail e
                  , "  sqlErrorHint: "
                  , bsToString $ sqlErrorHint e
                  ]

run :: Maybe Command -> Bool-> IO ()
run Nothing _ = printUsage stderr >> exitFailure
run (Just cmd) verbose =
    handleResult =<< case cmd of
        Initialize url tableName -> do
            con <- connectPostgreSQL (BS8.pack url)
            withTransaction con $ runMigration' $ MigrationContext'
                MigrationInitialization verbose con tableName
        Migrate url dir tableName -> do
            con <- connectPostgreSQL (BS8.pack url)
            withTransaction con $ runMigration' $ MigrationContext'
                (MigrationDirectory dir) verbose con tableName
        Validate url dir tableName -> do
            con <- connectPostgreSQL (BS8.pack url)
            withTransaction con $ runMigration' $ MigrationContext'
                (MigrationValidation (MigrationDirectory dir)) verbose con tableName
    where
        handleResult MigrationSuccess   = exitSuccess
        handleResult (MigrationError _) = exitFailure

parseCommand :: [String] -> Maybe Command
parseCommand ("init":url:tableName:_)         = Just (Initialize url (BS8.pack tableName))
parseCommand ("migrate":url:dir:tableName:_)  = Just (Migrate url dir (BS8.pack tableName))
parseCommand ("validate":url:dir:tableName:_) = Just (Validate url dir (BS8.pack tableName))
parseCommand ("init":url:_)         = Just (Initialize url "schema_migrations")
parseCommand ("migrate":url:dir:_)  = Just (Migrate url dir "schema_migrations")
parseCommand ("validate":url:dir:_) = Just (Validate url dir "schema_migrations")
parseCommand _                      = Nothing

printUsage :: Handle -> IO ()
printUsage = do
    putStrLn h "migrate [options] <command>"
    putStrLn h "  Options:"
    putStrLn h "      -h          Print help text"
    putStrLn h "      -q          Enable quiet mode"
    putStrLn h "  Commands:"
    putStrLn h "      init <con> {migrations table name}"
    putStrLn h "                  Initialize the database. Required to be run"
    putStrLn h "                  at least once."
    putStrLn h "                  {migrations table name} is the optiona name."
    putStrLn h "                  for the migrations table. This defaults to"
    putStrLn h "                  `schema_migrations`."
    putStrLn h "      migrate <con> <directory> {migrations table name}"
    putStrLn h "                  Execute all SQL scripts in the provided"
    putStrLn h "                  directory in alphabetical order."
    putStrLn h "                  Scripts that have already been executed are"
    putStrLn h "                  ignored. If a script was changed since the"
    putStrLn h "                  time of its last execution, an error is"
    putStrLn h "                  raised."
    putStrLn h "                  {migrations table name} is the optiona name."
    putStrLn h "                  for the migrations table. This defaults to"
    putStrLn h "                  `schema_migrations`."
    putStrLn h "      validate <con> <directory> {migrations table name}"
    putStrLn h "                  Validate all SQL scripts in the provided"
    putStrLn h "                  directory."
    putStrLn h "                  {migrations table name} is the optiona name."
    putStrLn h "                  for the migrations table. This defaults to"
    putStrLn h "                  `schema_migrations`."
    putStrLn h "      The <con> parameter is based on libpq connection string"
    putStrLn h "      syntax. Detailled information is available here:"
    putStrLn h "      <http://www.postgresql.org/docs/9.3/static/libpq-connect.html>"

data Command
    = Initialize String BS.ByteString
    | Migrate String FilePath BS.ByteString
    | Validate String FilePath BS.ByteString
    deriving (Show, Eq, Read, Ord)
