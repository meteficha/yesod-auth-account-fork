{-# LANGUAGE CPP, OverloadedStrings #-}
module BasicTests (basicSpecs) where

import Yesod.Test
import Foundation

redirectCode :: Int
#if MIN_VERSION_yesod_test(1,4,0)
redirectCode = 303
#else
redirectCode = 302
#endif

basicSpecs :: YesodSpec MyApp
basicSpecs =
    ydescribe "Basic tests" $ do
        yit "checks the home page is not logged in" $ do
            get' "/"
            statusIs 200
            bodyContains "Please visit the <a href=\"/auth/login\">Login page"

        yit "tests an invalid login" $ do
            get' "/auth/login"
            statusIs 200

            post' "/auth/page/account/login" $ do
                byLabel "Username" "abc"
                byLabel "Password" "xxx"

            statusIs redirectCode
            get' "/auth/login"
            statusIs 200
            bodyContains "Invalid username/password combination"

        yit "new account page looks ok" $ do
            get' "/auth/page/account/newaccount"
            statusIs 200
            htmlAllContain "title" "Register a new account"
            bodyContains "Register"

        yit "reset password page looks ok" $ do
            get' "/auth/page/account/resetpassword"
            statusIs 200
            bodyContains "Send password reset email"

            post' "/auth/page/account/resetpassword" $ do
                byLabel "Username" "abc"
                addToken

            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "Invalid username"

        yit "verify page returns an error" $ do
            get' "/auth/page/account/verify/abc/xxxxxx"
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "invalid verification key"

        yit "new password returns an error" $ do
            get' "/auth/page/account/newpassword/abc/xxxxxx"
            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "invalid verification key"

        yit "set password returns an error" $ do
            post' "/auth/page/account/setpassword" $ do
                addPostParam "f1" "xxx"
                addPostParam "f2" "xxx"
                addPostParam "f3" "xxx"
                addPostParam "f4" "xxx"
                addPostParam "f5" "xxx"

            statusIs redirectCode
            get' "/"
            statusIs 200
            bodyContains "As a protection against cross-site"

        yit "resend verify email returns an error" $ do
            post' "/auth/page/account/resendverifyemail" $ do
                addPostParam "f1" "xxx"
                addPostParam "f2" "xxx"

            statusIs 400
            bodyContains "As a protection against cross-site"
