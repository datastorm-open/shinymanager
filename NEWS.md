# shinymanager 1.2.0

## Security

* Admin operations (add, edit, delete users, change or reset passwords, downloads, logs export) are
  now checked on the server and need a valid (not revoked, not timed out) admin token. Before, the
  admin module was started for every session and its inputs were accepted without authentication
  (since 1.1.0).
* A revoked token (logout, timeout) no longer keeps the user identity, the password change needs a
  valid token, and logout revokes the token even when the logs can't be written.

## New features

* Self-service password reset by email: `options("shinymanager.reset_password" = TRUE)` adds a
  "Forgot password?" link on the authentication page (username + email, or username only with
  `"username"`). A temporary password is sent with the `send_mail` function given to
  `secure_server()` and must be changed on next login. Needs an `email` column (or
  `shinymanager.email_column`) and a SQLite / SQL backend. Related options:
  `shinymanager.reset_password_validity` (minutes), `shinymanager.reset_password_max` (resets in a
  row), `shinymanager.reset_password_cooldown` (seconds between two emails, default `60`, per R
  process) and `shinymanager.reset_password_unlock` (default `TRUE`). The optional `pwd_mngt`
  columns (`temp_pwd_expire`, `n_user_reset`) are created with SQLite, to add manually with SQL.
  The temporary password replaces the current one as soon as the email is sent: see
  `?secure_server` for the recommended settings.
* `send_smtp_mail()`: optional SMTP helper based on `emayili` (Suggests), with `timeout` and
  `max_times`.
* Logs dashboard: users overview (last connection and application, connections, wrong passwords,
  resets, locked) and security events per day. Every reset attempt is logged
  (`Reset password: <reason>`).
* Admin: `Locked` column in the passwords table when `shinymanager.pwd_failure_limit` is set.
* (#222) Italian language. Thanks @stewerner

## Behavior changes

* `check_credentials` is evaluated when the session starts: a configuration error (like a missing
  SQLite / .yml file, now with a dedicated message) shows when the login page loads.
* SQLite: the credentials are read on each login attempt, so a reset or an admin change is taken
  into account immediately.
* An account is locked at the attempt that reaches `shinymanager.pwd_failure_limit` (logged as
  `Account locked`).
* With `options("shinymanager.write_logs" = FALSE)`, failed connections are no longer written and
  the wrong password counter is reset on a successful login.
* New dependency: `later` (Imports).
* No database re-initialization is needed to upgrade (see README, *Upgrade from 1.1.x*). Once the
  optional `pwd_mngt` columns exist in a SQLite database, a 1.1.x version can't add users anymore.

## Bug fixes

* Login and password change with the Enter key right after typing: the typed values are sent
  before the click (complements #195 and #216).
* A missing (`NA`) wrong password counter or `must_change` in `pwd_mngt` no longer stops the login.
* SQLite: adding a user (admin) and writing the logs no longer fail when the `pwd_mngt` or `logs`
  table has additional columns (custom ones, or the new optional ones): they are filled with `NA`.
* SQL backend: a logs error at login or logout no longer stops the session (warning).
* Logs dashboard: the application selector lists the applications with failed connections only,
  and failed connections of a user on the same day are no longer merged.
* Translations: "Maximum number of users", "Failed to update user", "Your account is locked"
  (pt-BR, es, ja), and the Norwegian label "Maximum number of users: %s".

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
      
      
