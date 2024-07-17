# shinymanager 1.0.510

* Improve global performance
* Add sparklyr support
* (#188) FEAT : disable write logs and see logs
* (#182 & #187) FIX quiclky bind enter on auth
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
      
      
