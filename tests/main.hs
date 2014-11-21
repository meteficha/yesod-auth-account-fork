{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yesod
import Foundation
import Test.Hspec
import Yesod.Test
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)

import BasicTests
import NewAccount
import ChangePasswordLogged

main :: IO ()
main = withSqlitePool "test.db3" 10 $ \pool -> do
          runStderrLoggingT $ runSqlPool (runMigration migrateAll) pool
          runResourceT $ runNoLoggingT $ runSqlPool (deleteWhere ([] :: [Filter User])) pool
          hspec $ yesodSpec (MyApp pool) $ do
              basicSpecs
              newAccountSpecs
              changePasswordLoggedSpecs
