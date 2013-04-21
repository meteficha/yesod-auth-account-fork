{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yesod
import Foundation
import Yesod.Test
import Data.IORef
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)

import BasicTests

main :: IO ()
main = do
    verify <- newIORef ("", "", "")
    newPwd <- newIORef ("", "", "")

    withSqlitePool "test.db3" 10 $ \pool -> do
        runStderrLoggingT $ runSqlPool (runMigration migrateAll) pool
        let myapp = MyApp pool verify newPwd
        app <- toWaiAppPlain myapp
        runTests app pool basicSpecs
