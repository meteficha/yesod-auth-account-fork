# 1.4.0

* Add proper support for i18n:
    * Moved messages into Yesod.Auth.Account.Message module
    * Added renderAccountMessage function to YesodAuthAccount class, which defaults to
      the english messages.
    * resendVerifyEmailWidget, resetPasswordWidget, newAccountForm, resetPasswordForm, and
      runAccountPersistDB had their context's updated with a constraint for
      `YesodAuthAccount db master`, which shouldn't be a problem.

* Use the nonce package for generating the keys sent in the verification and reset
  password emails.  The `nonce` package provides efficient and cryptographically secure
  nonces.

* Support yesod 1.4 and persistent 2.1 (also bump our version to 1.4 to match yesod)
