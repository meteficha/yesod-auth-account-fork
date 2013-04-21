{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yesod
import Foundation
import Yesod.Test
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)

import BasicTests
import NewAccount

main :: IO ()
main = withSqlitePool "test.db3" 10 $ \pool -> do
          runStderrLoggingT $ runSqlPool (runMigration migrateAll) pool
          runResourceT $ runStderrLoggingT $ runSqlPool (deleteWhere ([] :: [Filter User])) pool
          let myapp = MyApp pool
          app <- toWaiAppPlain myapp
          runTests app pool basicSpecs
          runTests app pool newAccountSpecs
