{-# LANGUAGE CPP, OverloadedStrings #-}

module Main where

import Yesod
import Foundation
import Test.Hspec
import Yesod.Test
import Database.Persist.Sqlite
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)

import BasicTests
import NewAccount
import ChangePasswordLogged

main :: IO ()
#if MIN_VERSION_persistent(2,1,0)
main = runNoLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> do
#else
main = withSqlitePool "test.db3" 10 $ \pool -> runNoLoggingT $ do
#endif
          runSqlPool (runMigration migrateAll) pool
          runResourceT $ runSqlPool (deleteWhere ([] :: [Filter User])) pool
          liftIO $ hspec $ yesodSpec (MyApp pool) $ do
              basicSpecs
              newAccountSpecs
              changePasswordLoggedSpecs
