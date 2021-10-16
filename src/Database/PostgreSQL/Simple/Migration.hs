-- |
-- Module      : Database.PostgreSQL.Simple.Migration
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A migration library for postgresql-simple.
--
-- For usage, see Readme.markdown.

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Database.PostgreSQL.Simple.Migration
    (
    -- * Migration actions
      defaultOptions
    , runMigration
    , runMigrations
    , sequenceMigrations

    -- * Migration types
    , Checksum
    , MigrationOptions(..)
    , MigrationCommand(..)
    , MigrationResult(..)
    , ScriptName
    , Verbosity(..)

    -- * Migration result actions
    , getMigrations
    , getMigrations'

    -- * Migration result types
    , SchemaMigration(..)
    ) where

import           Control.Monad (void, when)
import qualified Crypto.Hash.MD5 as MD5 (hash)
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import qualified Data.ByteString.Base64 as B64 (encode)
import           Data.Functor ((<&>))
import           Data.List (isPrefixOf, sort)
import           Data.Time (LocalTime)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn, hPutStrLn)
import           Data.String (fromString)
import           Database.PostgreSQL.Simple ( Connection
                                            , Only (..)
                                            , execute
                                            , execute_
                                            , query
                                            , query_
                                            )
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.ToRow (ToRow (..))
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Database.PostgreSQL.Simple.Util (existsTable)
import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>))
import           System.IO (stderr)

-- | Executes migrations inside the provided 'MigrationContext'.
--
-- Returns 'MigrationSuccess' if the provided 'MigrationCommand' executes
-- without error. If an error occurs, execution is stopped and
-- a 'MigrationError' is returned.
--
-- It is recommended to wrap 'runMigration' inside a database transaction.
runMigration :: Connection -> MigrationOptions -> MigrationCommand -> IO (MigrationResult String)
runMigration con opts cmd =
  case cmd of
    MigrationInitialization ->
      initializeSchema con opts >> pure MigrationSuccess
    MigrationDirectory path ->
      executeDirectoryMigration con opts path
    MigrationScript name contents ->
      executeMigration con opts name contents
    MigrationFile name path ->
      executeMigration con opts name =<< BS.readFile path
    MigrationValidation validationCmd ->
      executeValidation con opts validationCmd
    MigrationCommands commands ->
      runMigrations con opts commands


-- | Execute a sequence of migrations
--
-- Returns 'MigrationSuccess' if all of the provided 'MigrationCommand's
-- execute without error. If an error occurs, execution is stopped and the
-- 'MigrationError' is returned.
--
-- It is recommended to wrap 'runMigrations' inside a database transaction.
runMigrations
  :: Connection -- ^ The postgres connection to use
  -> MigrationOptions -- ^ The options for this migration
  -> [MigrationCommand] -- ^ The commands to run
  -> IO (MigrationResult String)
runMigrations con opts commands =
  sequenceMigrations [runMigration con opts c | c <- commands]


-- | Run a sequence of contexts, stopping on the first failure
sequenceMigrations
    :: Monad m
    => [m (MigrationResult e)]
    -> m (MigrationResult e)
sequenceMigrations = \case
  [] -> pure MigrationSuccess
  c:cs -> do
    r <- c
    case r of
      MigrationError s -> pure (MigrationError s)
      MigrationSuccess -> sequenceMigrations cs

-- | Executes all SQL-file based migrations located in the provided 'dir'
-- in alphabetical order.
executeDirectoryMigration
  :: Connection
  -> MigrationOptions
  -> FilePath
  -> IO (MigrationResult String)
executeDirectoryMigration con opts dir =
  scriptsInDirectory dir >>= go
  where
    go fs = sequenceMigrations (executeMigrationFile <$> fs)
    executeMigrationFile f =
      BS.readFile (dir </> f) >>= executeMigration con opts f


-- | Lists all files in the given 'FilePath' 'dir' in alphabetical order.
scriptsInDirectory :: FilePath -> IO [String]
scriptsInDirectory dir =
  getDirectoryContents dir <&> (sort . filter (\x -> not $ "." `isPrefixOf` x))


-- | Executes a generic SQL migration for the provided script 'name' with content 'contents'.
executeMigration
  :: Connection
  -> MigrationOptions
  -> ScriptName
  -> BS.ByteString
  -> IO (MigrationResult String)
executeMigration con opts name contents = do
  let checksum = md5Hash contents
  checkScript con opts name checksum >>= \case
    ScriptOk -> do
      when (verbose opts) $ optLogWriter opts $ Right $ "Ok:\t" <> fromString name
      pure MigrationSuccess
    ScriptNotExecuted -> do
      void $ execute_ con (Query contents)
      void $ execute con q (name, checksum)
      when (verbose opts) $ optLogWriter opts $ Right ("Execute:\t" <> fromString name)
      pure MigrationSuccess
    ScriptModified { actual, expected } -> do
      when (verbose opts) $ optLogWriter opts $ Left ("Fail:\t" <> fromString name <> "\n" <> scriptModifiedErrorMessage expected actual)
      pure (MigrationError name)
  where
    q = "insert into " <> Query (optTableName opts) <> "(filename, checksum) values(?, ?)"

-- | Initializes the database schema with a helper table containing
-- meta-information about executed migrations.
initializeSchema :: Connection -> MigrationOptions -> IO ()
initializeSchema con opts = do
  when (verbose opts) $ optLogWriter opts $ Right "Initializing schema"
  void . execute_ con $ mconcat
      [ "create table if not exists " <> Query (optTableName opts) <> " "
      , "( filename varchar(512) not null"
      , ", checksum varchar(32) not null"
      , ", executed_at timestamp without time zone not null default now() "
      , ");"
      ]


-- | Validates a 'MigrationCommand'. Validation is defined as follows for these types:
--
-- * 'MigrationInitialization': validate the presence of the meta-information table.
-- * 'MigrationDirectory': validate the presence and checksum of all scripts found in the given directory.
-- * 'MigrationScript': validate the presence and checksum of the given script.
-- * 'MigrationFile': validate the presence and checksum of the given file.
-- * 'MigrationValidation': always succeeds.
-- * 'MigrationCommands': validates all the sub-commands stopping at the first failure.
executeValidation
  :: Connection
  -> MigrationOptions
  -> MigrationCommand
  -> IO (MigrationResult String)
executeValidation con opts cmd = case cmd of
  MigrationInitialization ->
    existsTable con (BS8.unpack $ optTableName opts) >>= \r -> pure $ if r
      then MigrationSuccess
      else MigrationError ("No such table: " <> BS8.unpack (optTableName opts))
  MigrationDirectory path ->
    scriptsInDirectory path >>= goScripts path
  MigrationScript name contents ->
    validate name contents
  MigrationFile name path ->
    validate name =<< BS.readFile path
  MigrationValidation _ ->
    pure MigrationSuccess
  MigrationCommands cs ->
    sequenceMigrations (executeValidation con opts <$> cs)
  where
    validate name contents =
      checkScript con opts name (md5Hash contents) >>= \case
        ScriptOk -> do
          when (verbose opts) $ optLogWriter opts (Right $ "Ok:\t" <> fromString name)
          pure MigrationSuccess
        ScriptNotExecuted -> do
          when (verbose opts) $ optLogWriter opts (Left $ "Missing:\t" <> fromString name)
          pure (MigrationError $ "Missing: " <> name)
        ScriptModified { expected, actual } -> do
          when (verbose opts) $ optLogWriter opts (Left $ "Checksum mismatch:\t" <> fromString name <> "\n" <> scriptModifiedErrorMessage expected actual)
          pure (MigrationError $ "Checksum mismatch: " <> name)

    goScripts path xs = sequenceMigrations (goScript path <$> xs)
    goScript path x = validate x =<< BS.readFile (path </> x)


-- | Checks the status of the script with the given name 'name'.
-- If the script has already been executed, the checksum of the script
-- is compared against the one that was executed.
-- If there is no matching script entry in the database, the script
-- will be executed and its meta-information will be recorded.
checkScript :: Connection -> MigrationOptions -> ScriptName -> Checksum -> IO CheckScriptResult
checkScript con opts name fileChecksum =
  query con q (Only name) >>= \case
    [] ->
      pure ScriptNotExecuted
    Only dbChecksum:_ | fileChecksum == dbChecksum ->
      pure ScriptOk
    Only dbChecksum:_ ->
      pure $ ScriptModified
        { expected = dbChecksum
        , actual = fileChecksum
        }
  where
    q = mconcat
        [ "select checksum from " <> Query (optTableName opts) <> " "
        , "where filename = ? limit 1"
        ]

-- | Calculates the MD5 checksum of the provided bytestring in base64
-- encoding.
md5Hash :: BS.ByteString -> Checksum
md5Hash = B64.encode . MD5.hash

-- | The checksum type of a migration script.
type Checksum = BS.ByteString

-- | The name of a script. Typically the filename or a custom name
-- when using Haskell migrations.
type ScriptName = String

-- | 'MigrationCommand' determines the action of the 'runMigration' script.
data MigrationCommand
  = MigrationInitialization
  -- ^ Initializes the database with a helper table containing meta
  -- information.
  | MigrationDirectory FilePath
  -- ^ Executes migrations based on SQL scripts in the provided 'FilePath'
  -- in alphabetical order.
  | MigrationFile ScriptName FilePath
  -- ^ Executes a migration based on script located at the provided
  -- 'FilePath'.
  | MigrationScript ScriptName BS.ByteString
  -- ^ Executes a migration based on the provided bytestring.
  | MigrationValidation MigrationCommand
  -- ^ Validates that the provided MigrationCommand has been executed.
  | MigrationCommands [MigrationCommand]
  -- ^ Performs a series of 'MigrationCommand's in sequence.
  deriving (Show, Eq, Read, Ord)

instance Semigroup MigrationCommand where
  (<>) (MigrationCommands xs) (MigrationCommands ys) = MigrationCommands (xs <> ys)
  (<>) (MigrationCommands xs) y = MigrationCommands (xs <> [y])
  (<>) x (MigrationCommands ys) = MigrationCommands (x : ys)
  (<>) x y = MigrationCommands [x, y]

instance Monoid MigrationCommand where
  mempty = MigrationCommands []
  mappend = (<>)

--data ExpectedVsActual a = ExpectedVsActual
--  { evaExpected :: !a
--  , evaActual :: !a
--  }

-- | A sum-type denoting the result of a single migration.
data CheckScriptResult
  = ScriptOk
  -- ^ The script has already been executed and the checksums match.
  -- This is good.
  | ScriptModified { expected :: Checksum, actual :: Checksum }
  -- ^ The script has already been executed and there is a checksum
  -- mismatch. This is bad.
  | ScriptNotExecuted
  -- ^ The script has not been executed, yet. This is good.
  deriving (Show, Eq, Read, Ord)

scriptModifiedErrorMessage :: Checksum -> Checksum -> T.Text
scriptModifiedErrorMessage expected actual =
  "expected: " <> fromString (show expected) <> "\nhash was: " <> fromString (show actual)

-- | A sum-type denoting the result of a migration.
data MigrationResult a
  = MigrationError a
  -- ^ There was an error in script migration.
  | MigrationSuccess
  -- ^ All scripts have been executed successfully.
  deriving (Show, Eq, Read, Ord, Functor, Foldable, Traversable)

data Verbosity
  = Verbose
  | Quiet
  deriving (Show, Eq)


data MigrationOptions = MigrationOptions
  { optVerbose :: !Verbosity
  -- ^ Verbosity of the library.
  , optTableName :: !BS.ByteString
  -- ^ The name of the table that stores the migrations, usually "schema_migrations"
  , optLogWriter :: Either T.Text T.Text -> IO ()
  -- ^ Logger. 'Either' indicates log level,
  -- 'Left' for an error message and 'Right' for an info message.
  }

defaultOptions :: MigrationOptions
defaultOptions =
  MigrationOptions
    { optVerbose = Quiet
    , optTableName = "schema_migrations"
    , optLogWriter = either (T.hPutStrLn stderr) T.putStrLn
    }

verbose :: MigrationOptions -> Bool
verbose o = optVerbose o == Verbose

-- | Produces a list of all executed 'SchemaMigration's in the default schema_migrations table
getMigrations :: Connection -> IO [SchemaMigration]
getMigrations con = getMigrations' con "schema_migrations"

-- | Produces a list of all executed 'SchemaMigration's.
getMigrations' :: Connection -> BS.ByteString -> IO [SchemaMigration]
getMigrations' con tableName = query_ con q
  where q = mconcat
          [ "select filename, checksum, executed_at "
          , "from " <> Query tableName <> " order by executed_at asc"
          ]

-- | A product type representing a single, executed 'SchemaMigration'.
data SchemaMigration = SchemaMigration
  { schemaMigrationName       :: BS.ByteString
  -- ^ The name of the executed migration.
  , schemaMigrationChecksum   :: Checksum
  -- ^ The calculated MD5 checksum of the executed script.
  , schemaMigrationExecutedAt :: LocalTime
  -- ^ A timestamp without timezone of the date of execution of the script.
  } deriving (Show, Eq, Read)

instance Ord SchemaMigration where
  compare (SchemaMigration nameLeft _ _) (SchemaMigration nameRight _ _) =
    compare nameLeft nameRight

instance FromRow SchemaMigration where
  fromRow = SchemaMigration <$>
    field <*> field <*> field

instance ToRow SchemaMigration where
  toRow (SchemaMigration name checksum executedAt) =
   [toField name, toField checksum, toField executedAt]
