This package provides a [Yesod](http://www.yesodweb.com/) authentication plugin for accounts. Each
account consists of an username, email, and password.  When initially creating an account, the email
is verified by sending a link in an email.  The plugin also supports password reset via email.

The plugin provides default pages implementing all of this functionality, but it has been designed
to allow all the pages (new account page, password reset, etc.) to be customized or for the forms to
be embedded into your own pages allowing you to just ignore the routes inside the plugin.  The
details are contained in the [haddock
documentation](http://hackage.haskell.org/package/yesod-auth-account-fork).

The plugin supports any form data storage by requiring you to implement a couple of interfaces for
data access.  The plugin has instances of these interfaces using persistent, but you can create your
own implementation if you are not using persistent or want more control over user data access and
storage.

A complete working example using persistent is
`example.hs`.  Also, see the
[haddock documentation](http://hackage.haskell.org/package/yesod-auth-account-fork).

If you use `stack`, you can run the example like this: `stack example.hs`.

# Fork

This is a fork of the `yesod-auth-account` package with the
following additions:

  * Login using either username or e-mail.

  * JSON support for single page applications.

We'd like to merge these changes back upstream but its
maintainer has been unresponsive.
