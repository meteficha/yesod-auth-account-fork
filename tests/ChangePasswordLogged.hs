{-# LANGUAGE CPP, OverloadedStrings #-}
module ChangePasswordLogged (changePasswordLoggedSpecs) where

import Yesod.Auth
import Yesod.Test
import Foundation
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
f1 :: T.Text
#if MIN_VERSION_yesod_form(1,3,9)
f1 = "f1"
#else
f1 = "f2"
#endif

changePasswordLoggedSpecs :: YesodSpec MyApp
changePasswordLoggedSpecs =
    ydescribe "Change Password while logged in tests" $ do
        yit "changes a password while logged in" $ do
            get' "/auth/page/account/newaccount"
            statusIs 200

            post'"/auth/page/account/newaccount" $ do
                addToken
                byLabel "Username" "aaa"
                byLabel "Email" "tst@example.com"
                byLabel "Password" "xxx"
                byLabel "Confirm" "xxx"

            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "A confirmation e-mail has been sent to tst@example.com"

            (username, email, verify) <- lastVerifyEmail
            assertEqual "username" username "aaa"
            assertEqual "email" email "tst@example.com"

            -- valid login
            get' "/auth/login"
            post'"/auth/page/account/login" $ do
                byLabel "Username" "aaa"
                byLabel "Password" "xxx"
            statusIs 200
            bodyContains "Your email has not yet been verified"

            -- resend verify email
            post'"/auth/page/account/resendverifyemail" $ do
                addToken
                addPostParam f1 "aaa" -- username is also a hidden field
            statusIs redirectCode
            get' "/"
            bodyContains "A confirmation e-mail has been sent to tst@example.com"

            (username', email', verify') <- lastVerifyEmail
            assertEqual "username" username' "aaa"
            assertEqual "email" email' "tst@example.com"
            assertEqual "verify" True (verify /= verify')

            -- verify email
            get' verify'
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "You are logged in as aaa"

            post $ AuthR LogoutR
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "Please visit the <a href=\"/auth/login\">Login page"

            -- valid login
            get' "/auth/login"
            post'"/auth/page/account/login" $ do
                byLabel "Username" "aaa"
                byLabel "Password" "xxx"
            statusIs redirectCode
            get' "/"
            bodyContains "You are logged in as aaa"

            -- change password while logged in
            -- good key
            get' "/auth/page/account/newpasswordlgd"
            post'"/auth/page/account/setpassword" $ do
                addToken
                byLabel "Please fill in your current password" "xxx"
                byLabel "New password" "www"
                byLabel "Confirm" "www"
                addPostParam f1 "aaa"
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "Password updated"
            bodyContains "You are logged in as aaa"

            post $ AuthR LogoutR

            -- check new password
            get' "/auth/login"
            post'"/auth/page/account/login" $ do
                byLabel "Username" "aaa"
                byLabel "Password" "www"
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "You are logged in as aaa"

            -- logout
            post $ AuthR LogoutR


        yit "cannot change password while logged out" $ do
            get' "/auth/page/account/newpasswordlgd"
            statusIs 403
