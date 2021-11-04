-- |
-- Module      : Main
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The test entry-point for postgresql-migration.

{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Database.PostgreSQL.Simple               (connectPostgreSQL)
import qualified Database.PostgreSQL.Simple.MigrationTestV1Compat as V1
import qualified Database.PostgreSQL.Simple.MigrationTest as V2
import qualified Database.PostgreSQL.Simple.TransactionPerRunTest as V2TrnRun
import qualified Database.PostgreSQL.Simple.TransactionPerStepTest as V2TrnStep
import           Database.PostgreSQL.Simple.Util          (withTransactionRolledBack)
import           Test.Hspec                               (hspec)

main :: IO ()
main = do
    conRollback <- connectPostgreSQL ""
    withTransactionRolledBack conRollback (hspec (V2.migrationSpec conRollback))
    withTransactionRolledBack conRollback (hspec (V1.migrationSpec conRollback))
    withTransactionRolledBack conRollback (hspec (V2TrnRun.migrationSpec conRollback))

    conPerStep <- connectPostgreSQL ""
    hspec (V2TrnStep.migrationSpec conPerStep)
