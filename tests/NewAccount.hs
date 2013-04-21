{-# LANGUAGE OverloadedStrings #-}
module NewAccount (newAccountSpecs) where

import Yesod.Test
import Foundation
import Database.Persist.Sqlite

newAccountSpecs :: SpecsConn Connection
newAccountSpecs =
    describe "New account tests" $ do
        it "new account with mismatched passwords" $ do
            get_ "/auth/page/account/newaccount"
            statusIs 200
            bodyContains "Register"

            post "/auth/page/account/newaccount" $ do
                addNonce
                byLabel "Username" "abc"
                byLabel "Email" "test@example.com"
                byLabel "Password" "xxx"
                byLabel "Confirm" "yyy"

            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "Passwords did not match"

        it "creates a new account" $ do
            get_ "/auth/page/account/newaccount"
            statusIs 200

            post "/auth/page/account/newaccount" $ do
                addNonce
                byLabel "Username" "abc"
                byLabel "Email" "test@example.com"
                byLabel "Password" "xxx"
                byLabel "Confirm" "xxx"

            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "A confirmation e-mail has been sent to test@example.com"

            (username, email, verify) <- lastVerifyEmail
            assertEqual "username" username "abc"
            assertEqual "email" email "test@example.com"

            get_ "/auth/page/account/verify/abc/zzzzzz"
            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "invalid verification key"
