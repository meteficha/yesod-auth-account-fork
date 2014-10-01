{-# LANGUAGE CPP, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}

module Foundation where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist.Sqlite
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Yesod
import Yesod.Auth
import Yesod.Auth.Account
import Yesod.Test

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

data MyApp = MyApp ConnectionPool

lastVerifyEmailR :: IORef (Username, Text, Text) -- ^ (username, email, verify url)
{-# NOINLINE lastVerifyEmailR #-}
lastVerifyEmailR = unsafePerformIO (newIORef ("", "", ""))

lastNewPwdEmailR :: IORef (Username, Text, Text) -- ^ (username, email, verify url)
{-# NOINLINE lastNewPwdEmailR #-}
lastNewPwdEmailR = unsafePerformIO (newIORef ("", "", ""))

lastVerifyEmail :: MonadIO m => m (Username, Text, Text)
lastVerifyEmail = liftIO $ readIORef lastVerifyEmailR

lastNewPwdEmail :: MonadIO m => m (Username, Text, Text)
lastNewPwdEmail = liftIO $ readIORef lastNewPwdEmailR

mkYesod "MyApp" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod MyApp

instance RenderMessage MyApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist MyApp where
#if MIN_VERSION_yesod(1,4,0)
    type YesodPersistBackend MyApp = SqlBackend
#else
    type YesodPersistBackend MyApp = SqlPersistT
#endif
    runDB action = do
        MyApp pool <- getYesod
        runSqlPool action pool

instance YesodAuth MyApp where
    type AuthId MyApp = Username
    getAuthId = return . Just . credsIdent
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [accountPlugin]
    authHttpManager _ = error "No manager needed"
    onLogin = return ()
    maybeAuthId = lookupSession "_ID"

instance AccountSendEmail MyApp where
    sendVerifyEmail name email url =
        liftIO $ writeIORef lastVerifyEmailR (name, email, url)

    sendNewPasswordEmail name email url =
        liftIO $ writeIORef lastNewPwdEmailR (name, email, url)

instance YesodAuthAccount (AccountPersistDB MyApp User) MyApp where
    runAccountDB = runAccountPersistDB

getHomeR :: Handler Html
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

-- Temporary helpers for testing
get' :: Yesod site => Text -> YesodExample site ()
get' url = Yesod.Test.get url

post' :: Yesod site => Text -> RequestBuilder site () -> YesodExample site ()
post' url builder = request $ do
    setUrl url
    setMethod "POST"
    builder
