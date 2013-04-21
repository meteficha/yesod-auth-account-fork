{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}

import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Yesod
import Yesod.Auth
import YesodAuthAccount

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

instance AccountSendEmail MyApp

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

main :: IO ()
main = withSqlitePool "test.db3" 10 $ \pool -> do
    runStderrLoggingT $ runSqlPool (runMigration migrateAll) pool
    warpDebug 3000 $ MyApp pool

