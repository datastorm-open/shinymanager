# shinymanager 1.1.1.1

* SECURITY FIX: the admin operations (add, edit, delete users, change or reset passwords,
  downloads) are now checked on the server: they need a valid (not revoked, not timed out) admin
  token. Before, the admin module was started for every session and its inputs were accepted
  without authentication (since 1.1.0).
* SECURITY FIX: a revoked token (logout, timeout) no longer keeps the user identity, and the
  password change checks that the token is still valid.
* SECURITY FIX: the logs download also checks on the server that the admin token is still valid.
* SECURITY FIX: password reset: no new email for a user who received one recently, checked on the
  server (the button was only disabled in the browser). Logged as `Reset password: too soon`. The
  delay is set with `options("shinymanager.reset_password_cooldown")`, in seconds (default `60`,
  `0` for no limit). Kept in memory, per R process.
* FIX: password reset: the temporary password is not saved when the password or the email of the
  account were modified (by an admin) while sending the email.
* FIX: password reset: the stored email is trimmed before sending.
* FIX: logout revokes the token even when the logs can't be written. SQL backend: a logs error at
  login or logout no longer stops the session (warning, as with SQLite).
* FIX: a missing (`NA`) wrong password counter or `must_change` in `pwd_mngt` no longer stops the
  login.
* FIX: password reset transaction: an error when starting the transaction now stops the save
  (nothing is written outside a transaction), except for the backends without transactions
  (BigQuery, Spark).
* FIX: password reset refused, before sending the email (logged as `Reset password: db error`),
  when the user has no row in `pwd_mngt` (the password would not be temporary) or when the SQL
  column `temp_pwd_expire` is not a text column.
* FIX: password reset: a saving error shows a dedicated message, and a database error no longer
  stops the session.
* FIX: SQL backend without email column: the same answer for known and unknown users.
* FIX: login (and password change) with the Enter key right after typing: the typed values are
  sent before the click. Shiny sends text inputs 250 ms after the last key, so the server could
  check an outdated password and log a wrong password (complements #195 and #216).
* FIX: password reset with an empty field: a "Please fill in all fields." message (translated)
  instead of the generic message, nothing checked nor sent.
* `auth_server()` now evaluates `check_credentials` when the session starts (it was at the first
  login), to know the database used: a configuration error now shows when the login page loads.
  `check_credentials()` gives a dedicated message (with the working directory) when the SQLite or
  .yml file does not exist.
* FIX: the "Forgot password?" link is hidden when the reset can't work (data.frame backend, no
  `send_mail` function).
* The reset button is enabled again after the same minimal delay (5 seconds) whatever the
  outcome, so the response time does not reveal which accounts exist (`later` added to Imports).
* FIX: logs dashboard: the application selector also lists the applications with failed
  connections only.
* FIX: admin labels "Maximum number of users" and "Failed to update user" are translated. The logs
  dashboard counter "Email errors" is renamed "Reset errors" (it also counts saving and
  configuration errors).
* DOC: the temporary password replaces the current one when the email is sent: recommendation to
  use the `TRUE` mode and to set `shinymanager.reset_password_max` knowing this risk.

* FEAT: optional email sending, with no mandatory dependency. Provide a `send_mail`
  function to `secure_server()` (any backend), or use the optional helper `send_smtp_mail()`
  (based on `emayili`, in Suggests).
* FEAT: self-service password reset from the authentication page, enabled with
  `options("shinymanager.reset_password" = TRUE)`. A user can reset its password only by
  providing a valid username and its associated email (requires an `email` column and a
  SQLite / SQL backend). A temporary password is emailed and the user is forced to change
  it on next login. Set the option to `"username"` to only ask for a username and send the
  temporary password to the email stored for that user. Reset attempts are recorded in the
  admin logs (`Reset password`, `Reset password: unknown user`, `... wrong email`, `... no email`).
  The `send_mail` function must raise an error when sending fails, otherwise the password would be
  reset without the user receiving it.
* FEAT: `options("shinymanager.reset_password_validity" = X)` limits the validity of the emailed
  temporary password to X minutes (default : no expiration). Stored as text (UTC) in a
  `temp_pwd_expire` column of `pwd_mngt` (created automatically with SQLite, to add manually as a
  text column with a SQL backend). Admin-generated passwords are not concerned; an admin reset
  cancels a pending emailed password. Login with an expired temporary password shows a dedicated
  message (translated) and is logged as `Reset password: expired`.
* FEAT: `options("shinymanager.reset_password_max" = N)` limits the number of password resets
  in a row (default : no limit), reset on the next successful login with a non temporary password
  or by an admin reset. Stored in a `n_user_reset` column of `pwd_mngt` (created automatically with
  SQLite, to add manually as an integer column with a SQL backend).
* FEAT: `options("shinymanager.reset_password_unlock" = FALSE)` leaves the unlocking of an account
  locked by `pwd_failure_limit` to an admin (default `TRUE` : a password reset unlocks it).
* FEAT: password reset feedback : the generic message is displayed before sending the email, the
  button is disabled meanwhile, an account that can't reset its password gets a dedicated message
  and a failed sending shows an error popup. No reset for an expired / unauthorized account.
  Sending and saving errors are raised as R warnings and every attempt is logged (`Reset password: <reason>`).
* FEAT: logs dashboard : users overview (last connection, last application, connections, wrong
  passwords, resets, locked) and security events (counters and failed events per day). Unknown
  usernames are only counted.
* FEAT: the account is shown as locked as soon as the wrong password reaching
  `pwd_failure_limit` is entered (logged as `Account locked`), and the admin passwords table shows
  a `Locked` column when a limit is set.
* FIX: with `options("shinymanager.write_logs" = FALSE)`, failed connections are no longer written in
  the SQLite logs, and the wrong password counter is now reset on a successful login.
* FIX: with a SQLite database, `check_credentials()` reads the credentials on each login attempt
  instead of once, so a password changed meanwhile (reset, admin) is taken into account.
* FIX: the mailed temporary password is saved in a single transaction (credentials and
  `pwd_mngt`, re-read after sending the email), so a failure leaves the account untouched and a
  change made meanwhile is not overwritten. Backends without transactions (BigQuery, Spark) keep
  separate writes.
* FIX: translations of "Your account is locked" (pt-BR, es, ja).
* FIX: logs dashboard no longer merges failed connections (without token) of a user on the same day.
* `send_smtp_mail()` gains `timeout` (seconds per attempt, default `30`) and `max_times`
  (sending attempts, default `1`) to bound the time an unresponsive SMTP server can block
  the R process.

# shinymanager 1.1.1

* (#222) Added Italian Language, Thanks @stewerner
* FIX Norwegian label "Maximum number of users: %s"

# shinymanager 1.1.0

* (#220) Added Norwegian Language, Thanks @mcldrchl
* (#212) Add MS SQL Server support, Thanks @ads40
* (#217) focus_input event fires too late, Thanks @ismirsehregal
* (#219) Malformed labels using latest shiny version, Thanks @ismirsehregal
* (#198) FIX duplicated input id 'shinymanager_language' (admin panel & authentication page)

# shinymanager 1.0.510

* Improve global performance
* Add sparklyr support
* (#188) FEAT : disable write logs and see logs
* (#182 & #187) FIX quickly bind enter on auth
* FEAT/FIX : SQL features (not only sqlite but Postgres, MySQL, ...)

# shinymanager 1.0.5

* (#154) add indonesian. Thanks @aswansyahputra 
* (#178) add chinese. Thanks @wtbxsjy
* FEAT : SQL features (not only sqlite but Postgres, MySQL, ...)

# shinymanager 1.0.410

* (#112) : fix bug changing user name. Thanks @StatisMike
* fix DT checkbox (rm/add user)
* Changed fab button z-index to make it appear above sidebar in shinydashboard/bs4Dash (fix [#123](https://github.com/datastorm-open/shinymanager/issues/123))
* can pass validate_pwd_ to secure_server
* (#129) add japanese. Thanks @ironwest 
* (#113) disable download db & logs. Thanks @StatisMike
* (#130) update somes icons. Thanks @ismirsehregal
* add download user file
* add options for validity pwd & update check same / old pwd
* add options for locked account
* (#144) Switch to Font Awesome 6 icon names. Thanks @ismirsehregal
* (#143) add Greek language. Thanks @lefkiospaikousis 

# shinymanager 1.0.400

* (#84) : new FAB button with a position argument
* (#81) : keep request query string. Thanks @erikor 
* (#24) : secure_server() : added keep_token arg 
* (#54) add spanish. Thanks @EAMI91 
* (#98) add german. Thanks @indubio
* (#106) add polish. Thanks @StatisMike
* (#39) : fix use shiny bookmarking
* Admin mode: new edit multiple users
* Add full language label using `choose_language`
* (#66) fix ``quanteda`` bad interaction 
* (#71) fix logs count for admin user
* (#26) : restrict number of users

# shinymanager 1.0.300

* Add ``autofocus`` on username input.
* Fix some (strange) bug with ``input$shinymanager_where``
* Fix `inputs_list` with some shiny version
* `auth_ui()` now accept a `choose_language` arguments.
* Rename `br` language into `pt-BR` (iso code)
* add user info in downloaded log file
* add `set_labels()` for customize labels
* Fix simultaneous admin session
* (#37) hashing password using `scrypt`

# shinymanager 1.0.200

* Can configure shiny input for editing user in `secure_app()` with new `inputs_list`.
* `auth_ui()` now accept a `background` argument change default css.
* `auth_ui()` : `tag_img` -> `tags_top`, `tag_div` -> `tags_bottom`
* (#18) IE compatibility (timeout & admin log)
* (#17) Add support to Brazilian Portuguese. Thanks to @erikson84

# shinymanager 1.0.100

* Fix bug on timeout.
* `secure_app()` now accept a `function` to define the UI.
* Disable `admin mode` if no SQL database
* `admin mode` : add possibility to export encrypt SQL

# shinymanager 1.0

* Now on CRAN
      
      
