{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}

module Foundation where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist.Sqlite
import Data.IORef
import Yesod
import Yesod.Auth
import Yesod.Auth.Account

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
User
    username Text
    UniqueUsername username
    password ByteString
    emailAddress Text
    verified Bool
    verifyKey Text
    resetPasswordKey Text
    deriving Show
|]

instance PersistUserCredentials User where
    userUsernameF = UserUsername
    userPasswordHashF = UserPassword
    userEmailF = UserEmailAddress
    userEmailVerifiedF = UserVerified
    userEmailVerifyKeyF = UserVerifyKey
    userResetPwdKeyF = UserResetPasswordKey
    uniqueUsername = UniqueUsername

    userCreate name email key pwd = User name pwd email False key ""

data MyApp = MyApp { connPool :: ConnectionPool
                   , lastVerifyEmailR :: IORef (Username, Text, Text) -- ^ (username, email, verify url)
                   , lastNewPwdEmailR :: IORef (Username, Text, Text) -- ^ (username, email, verify url)
                   }

lastVerifyEmail :: GHandler s MyApp (Username, Text, Text)
lastVerifyEmail = do
    app <- getYesod
    liftIO $ readIORef $ lastVerifyEmailR app

lastNewPwdEmail :: GHandler s MyApp (Username, Text, Text)
lastNewPwdEmail = do
    app <- getYesod
    liftIO $ readIORef $ lastNewPwdEmailR app

mkYesod "MyApp" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod MyApp

instance RenderMessage MyApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist MyApp where
    type YesodPersistBackend MyApp = SqlPersist
    runDB action = do
        app <- getYesod
        runSqlPool action $ connPool app

instance YesodAuth MyApp where
    type AuthId MyApp = Username
    getAuthId = return . Just . credsIdent
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [accountPlugin]
    authHttpManager _ = error "No manager needed"
    onLogin = return ()

instance AccountSendEmail MyApp where
    sendVerifyEmail name email url = do
        app <- getYesod
        liftIO $ writeIORef (lastVerifyEmailR app) (name, email, url)

    sendNewPasswordEmail name email url = do
        app <- getYesod
        liftIO $ writeIORef (lastNewPwdEmailR app) (name, email, url)

instance YesodAuthAccount (AccountPersistDB MyApp User) MyApp where
    runAccountDB = runAccountPersistDB

getHomeR :: Handler RepHtml
getHomeR = do
    maid <- maybeAuthId
    case maid of
        Nothing -> defaultLayout $ [whamlet|
<p>Please visit the <a href="@{AuthR LoginR}">Login page</a>
|]
        Just u -> defaultLayout $ [whamlet|
<p>You are logged in as #{u}
<p><a href="@{AuthR LogoutR}">Logout</a>
|]
