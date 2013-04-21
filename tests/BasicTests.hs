{-# LANGUAGE OverloadedStrings #-}
module BasicTests (basicSpecs) where

import Yesod.Test
import Database.Persist.Sqlite

basicSpecs :: SpecsConn Connection
basicSpecs =
    describe "Basic tests" $ do
        it "checks the home page is not logged in" $ do
            get_ "/"
            statusIs 200
            bodyContains "Please visit the <a href=\"/auth/login\">Login page"

        it "tests an invalid login" $ do
            get_ "/auth/login"
            statusIs 200

            post "/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "xxx"

            statusIs 303
            get_ "/auth/login"
            statusIs 200
            bodyContains "Invalid username or password"

        it "new account page looks ok" $ do
            get_ "/auth/page/account/newaccount"
            statusIs 200
            htmlAllContain "title" "Register a new account"
            bodyContains "Register"

        it "reset password page looks ok" $ do
            get_ "/auth/page/account/resetpassword"
            statusIs 200
            bodyContains "Send email to reset password"

            post "/auth/page/account/resetpassword" $ do
                byLabel "Username" "abc"
                addNonce

            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "Invalid username"

        it "verify page returns an error" $ do
            get_ "/auth/page/account/verify/abc/xxxxxx"
            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "invalid verification key"

        it "new password returns an error" $ do
            get_ "/auth/page/account/newpassword/abc/xxxxxx"
            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "invalid verification key"

        it "set password returns an error" $ do
            post "/auth/page/account/setpassword" $ do
                byName "f1" "xxx"
                byName "f2" "xxx"
                byName "f3" "xxx"
                byName "f4" "xxx"
                byName "f5" "xxx"

            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "As a protection against cross-site"

        it "resend verify email returns an error" $ do
            post "/auth/page/account/resendverifyemail" $ do
                byName "f1" "xxx"
                byName "f2" "xxx"

            statusIs 400
            bodyContains "As a protection against cross-site"
  
