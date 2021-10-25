-- |
-- Module      : Main
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The test entry-point for postgresql-simple-migration.

{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Database.PostgreSQL.Simple               (connectPostgreSQL)
import qualified Database.PostgreSQL.Simple.MigrationTestV1Compat as V1
import qualified Database.PostgreSQL.Simple.MigrationTest as V2
import           Database.PostgreSQL.Simple.Util          (withTransactionRolledBack)
import           Test.Hspec                               (hspec)

main :: IO ()
main = do
    con <- connectPostgreSQL "dbname=test host=localhost"
    withTransactionRolledBack con (hspec (V2.migrationSpec con))
    withTransactionRolledBack con (hspec (V1.migrationSpec con))
