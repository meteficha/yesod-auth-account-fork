{-# LANGUAGE OverloadedStrings #-}
module BasicTests (basicSpecs) where

import Yesod.Test
import Database.Persist.Sqlite

basicSpecs :: SpecsConn Connection
basicSpecs =
    describe "Some basic tests" $
        it "home page is not logged in" $ do
            get_ "/"
            statusIs 200
            bodyContains "Please visit the <a href=\"/auth/login\">Login page"
