# 1.3.0

* Add proper support for i18n:
    * Moved messages into Yesod.Auth.Account.Message module
    * Added renderAccountMessage function to YesodAuthAccount class, which defaults to
      the english messages.
    * resendVerifyEmailWidget, resetPasswordWidget, newAccountForm, resetPasswordForm, and
      runAccountPersistDB had their context's updated with a constraint for
      `YesodAuthAccount db master`, which shouldn't be a problem.

