{-# LANGUAGE OverloadedStrings #-}
module NewAccount (newAccountSpecs) where

import Yesod.Test
import Foundation
import Database.Persist.Sqlite
import Text.XML.Cursor (attribute)
import qualified Data.Text.Encoding as T

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

            -- try login
            get_ "/auth/login"
            statusIs 200
            post "/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "yyy"
            statusIs 303
            get_ "/auth/login"
            statusIs 200
            bodyContains "Invalid username or password"

            -- valid login
            post "/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "xxx"
            statusIs 200
            bodyContains "Your email has not yet been verified"

            -- resend verify email
            post "/auth/page/account/resendverifyemail" $ do
                addNonce
                byName "f2" "abc" -- username is also a hidden field
            statusIs 303
            get_ "/"
            bodyContains "A confirmation e-mail has been sent to test@example.com"

            (username', email', verify') <- lastVerifyEmail
            assertEqual "username" username' "abc"
            assertEqual "email" email' "test@example.com"
            assertEqual "verify" True (verify /= verify')

            -- verify email
            get_ $ T.encodeUtf8 verify'
            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "You are logged in as abc"

            post_ "/auth/logout"
            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "Please visit the <a href=\"/auth/login\">Login page"

            -- valid login
            get_ "/auth/login"
            post "/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "xxx"
            statusIs 303
            get_ "/"
            bodyContains "You are logged in as abc"

            -- logout
            post_ "/auth/logout"

            -- reset password
            get_ "/auth/page/account/resetpassword"
            statusIs 200
            bodyContains "Send email to reset password"
            post "/auth/page/account/resetpassword" $ do
                byLabel "Username" "abc"
                addNonce
            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "A password reset email has been sent to your email address"

            (username'', email'', newpwd) <- lastNewPwdEmail
            assertEqual "User" username'' "abc"
            assertEqual "Email" email'' "test@example.com"

            -- bad key
            get_ $ T.encodeUtf8 newpwd
            statusIs 200
            post "/auth/page/account/setpassword" $ do
                addNonce
                byLabel "New password" "www"
                byLabel "Confirm" "www"
                byName "f2" "abc"
                byName "f3" "qqqqqqqqqqqqqq"
            statusIs 403
            bodyContains "Invalid key"

            -- good key
            get_ $ T.encodeUtf8 newpwd
            statusIs 200
            post "/auth/page/account/setpassword" $ do
                addNonce
                byLabel "New password" "www"
                byLabel "Confirm" "www"
                byName "f2" "abc"
                matches <- htmlQuery "input[name=f3][type=hidden][value]"
                key <- case matches of
                          [] -> error "Unable to find set password key"
                          element:_ -> return $ head $ attribute "value" $ parseHTML element
                byName "f3" key
            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "Password updated"
            bodyContains "You are logged in as abc"

            post_ "/auth/logout"

            -- check new password
            get_ "/auth/login"
            post "/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "www"
            statusIs 303
            get_ "/"
            statusIs 200
            bodyContains "You are logged in as abc"
