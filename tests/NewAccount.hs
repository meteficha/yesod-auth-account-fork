{-# LANGUAGE CPP, OverloadedStrings #-}
module NewAccount (newAccountSpecs) where

import Data.Monoid
import Yesod.Auth
import Yesod.Test
import Foundation
import Text.XML.Cursor (attribute)
import qualified Data.Text as T

redirectCode :: Int
#if MIN_VERSION_yesod_test(1,4,0)
redirectCode = 303
#else
redirectCode = 302
#endif

-- In 9f379bc219bd1fdf008e2c179b03e98a05b36401 (which went into yesod-form-1.3.9)
-- the numbering of fields was changed.  We normally wouldn't care because fields
-- can be set via 'byLabel', but hidden fields have no label so we must use the id
-- directly. We temporarily support both versions of yesod form with the following.
f1, f2 :: T.Text
#if MIN_VERSION_yesod_form(1,3,9)
f1 = "f1"
f2 = "f2"
#else
f1 = "f2"
f2 = "f3"
#endif

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

            statusIs redirectCode
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

            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "A confirmation e-mail has been sent to test@example.com"

            (username, email, verify) <- lastVerifyEmail
            assertEqual "username" username "abc"
            assertEqual "email" email "test@example.com"

            get' "/auth/page/account/verify/abc/zzzzzz"
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "invalid verification key"

            -- try login
            get' "/auth/login"
            statusIs 200
            post'"/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "yyy"
            statusIs redirectCode
            get' "/auth/login"
            statusIs 200
            bodyContains "Invalid username/password combination"

            -- valid login
            post'"/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "xxx"
            statusIs 200
            bodyContains "Your email has not yet been verified"

            -- valid login with email
            get' "/auth/login"
            statusIs 200
            post'"/auth/page/account/login" $ do
                byLabel "Username" "test@example.com"
                byLabel "Password" "xxx"
            statusIs 200
            bodyContains "Your email has not yet been verified"


            -- resend verify email
            post'"/auth/page/account/resendverifyemail" $ do
                addNonce
                addPostParam f1 "abc" -- username is also a hidden field
            statusIs redirectCode
            get' "/"
            bodyContains "A confirmation e-mail has been sent to test@example.com"

            (username', email', verify') <- lastVerifyEmail
            assertEqual "username" username' "abc"
            assertEqual "email" email' "test@example.com"
            assertEqual "verify" True (verify /= verify')

            -- verify email
            get' verify'
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "You are logged in as abc"

            post $ AuthR LogoutR
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "Please visit the <a href=\"/auth/login\">Login page"

            -- valid login
            get' "/auth/login"
            post'"/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "xxx"
            statusIs redirectCode
            get' "/"
            bodyContains "You are logged in as abc"

            -- logout
            post $ AuthR LogoutR

            -- valid login with email
            get' "/auth/login"
            post'"/auth/page/account/login" $ do
                byLabel "Username" "test@example.com"
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
            statusIs redirectCode
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
                addPostParam f1 "abc"
                addPostParam f2 "qqqqqqqqqqqqqq"
            statusIs 403
            bodyContains "Invalid key"

            -- good key
            get' newpwd
            statusIs 200
            matches <- htmlQuery $ "input[name=" <> f2 <> "][type=hidden][value]"
            post'"/auth/page/account/setpassword" $ do
                addNonce
                byLabel "New password" "www"
                byLabel "Confirm" "www"
                addPostParam f1 "abc"
                key <- case matches of
                          [] -> error "Unable to find set password key"
                          element:_ -> return $ head $ attribute "value" $ parseHTML element
                addPostParam f2 key
            statusIs redirectCode
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
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "You are logged in as abc"

        yit "errors with a username with a period" $ do
            get' "/auth/page/account/newaccount"
            statusIs 200

            post' "/auth/page/account/newaccount" $ do
                addNonce
                byLabel "Username" "x.y"
                byLabel "Email" "xy@example.com"
                byLabel "Password" "hunter2"
                byLabel "Confirm" "hunter2"

            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "Invalid username" -- Issue #2: a valid username was not checked on creation
