{-# LANGUAGE OverloadedStrings #-}
module NewAccount (newAccountSpecs) where

import Yesod.Auth
import Yesod.Test
import Foundation
import Text.XML.Cursor (attribute)

newAccountSpecs :: YesodSpec MyApp
newAccountSpecs =
    ydescribe "New account tests" $ do
        yit "new account with mismatched passwords" $ do
            get' "/auth/page/account/newaccount"
            statusIs 200
            bodyContains "Register"

            post'"/auth/page/account/newaccount" $ do
                addNonce
                byLabel "Username" "abc"
                byLabel "Email" "test@example.com"
                byLabel "Password" "xxx"
                byLabel "Confirm" "yyy"

            statusIs 302
            get' "/"
            statusIs 200
            bodyContains "Passwords did not match"

        yit "creates a new account" $ do
            get' "/auth/page/account/newaccount"
            statusIs 200

            post'"/auth/page/account/newaccount" $ do
                addNonce
                byLabel "Username" "abc"
                byLabel "Email" "test@example.com"
                byLabel "Password" "xxx"
                byLabel "Confirm" "xxx"

            statusIs 302
            get' "/"
            statusIs 200
            bodyContains "A confirmation e-mail has been sent to test@example.com"

            (username, email, verify) <- lastVerifyEmail
            assertEqual "username" username "abc"
            assertEqual "email" email "test@example.com"

            get' "/auth/page/account/verify/abc/zzzzzz"
            statusIs 302
            get' "/"
            statusIs 200
            bodyContains "invalid verification key"

            -- try login
            get' "/auth/login"
            statusIs 200
            post'"/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "yyy"
            statusIs 302
            get' "/auth/login"
            statusIs 200
            bodyContains "Invalid username/password combination"

            -- valid login
            post'"/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "xxx"
            statusIs 200
            bodyContains "Your email has not yet been verified"

            -- resend verify email
            post'"/auth/page/account/resendverifyemail" $ do
                addNonce
                addPostParam "f2" "abc" -- username is also a hidden field
            statusIs 302
            get' "/"
            bodyContains "A confirmation e-mail has been sent to test@example.com"

            (username', email', verify') <- lastVerifyEmail
            assertEqual "username" username' "abc"
            assertEqual "email" email' "test@example.com"
            assertEqual "verify" True (verify /= verify')

            -- verify email
            get' verify'
            statusIs 302
            get' "/"
            statusIs 200
            bodyContains "You are logged in as abc"

            post $ AuthR LogoutR
            statusIs 302
            get' "/"
            statusIs 200
            bodyContains "Please visit the <a href=\"/auth/login\">Login page"

            -- valid login
            get' "/auth/login"
            post'"/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "xxx"
            statusIs 302
            get' "/"
            bodyContains "You are logged in as abc"

            -- logout
            post $ AuthR LogoutR

            -- reset password
            get' "/auth/page/account/resetpassword"
            statusIs 200
            bodyContains "Send password reset email"
            post'"/auth/page/account/resetpassword" $ do
                byLabel "Username" "abc"
                addNonce
            statusIs 302
            get' "/"
            statusIs 200
            bodyContains "A password reset email has been sent to your email address"

            (username'', email'', newpwd) <- lastNewPwdEmail
            assertEqual "User" username'' "abc"
            assertEqual "Email" email'' "test@example.com"

            -- bad key
            get' newpwd
            statusIs 200
            post'"/auth/page/account/setpassword" $ do
                addNonce
                byLabel "New password" "www"
                byLabel "Confirm" "www"
                addPostParam "f2" "abc"
                addPostParam "f3" "qqqqqqqqqqqqqq"
            statusIs 403
            bodyContains "Invalid key"

            -- good key
            get' newpwd
            statusIs 200
            matches <- htmlQuery "input[name=f3][type=hidden][value]"
            post'"/auth/page/account/setpassword" $ do
                addNonce
                byLabel "New password" "www"
                byLabel "Confirm" "www"
                addPostParam "f2" "abc"
                key <- case matches of
                          [] -> error "Unable to find set password key"
                          element:_ -> return $ head $ attribute "value" $ parseHTML element
                addPostParam "f3" key
            statusIs 302
            get' "/"
            statusIs 200
            bodyContains "Password updated"
            bodyContains "You are logged in as abc"

            post $ AuthR LogoutR

            -- check new password
            get' "/auth/login"
            post'"/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "www"
            statusIs 302
            get' "/"
            statusIs 200
            bodyContains "You are logged in as abc"
