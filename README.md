# shinymanager <img src="man/figures/shinymanager.png" width=200 align="right" />

[![Travis build status](https://travis-ci.org/datastorm-open/shinymanager.svg?branch=master)](https://travis-ci.org/datastorm-open/shinymanager)
[![version](http://www.r-pkg.org/badges/version/shinymanager)](https://CRAN.R-project.org/package=shinymanager)
[![cranlogs](http://cranlogs.r-pkg.org/badges/shinymanager)](https://CRAN.R-project.org/package=shinymanager)
[![cran checks](https://cranchecks.info/badges/worst/shinymanager)](https://cranchecks.info/pkgs/shinymanager)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

> Simple and secure authentication mechanism for single 'Shiny' applications. Credentials can be stored in an encrypted 'SQLite' database or on your own SQL Database (PostgreSQL, MySQL, ...). Source code of main application is protected until authentication is successful.

Live demo:

* On Shiny-server: http://shinyapps.dreamrs.fr/shinymanager-demo/
* On shinyapps.io: https://dreamrs.shinyapps.io/shinymanager-demo/

You can authenticate with:

* user: `shiny` / password: `shiny`
* user: `shinymanager` / password: `shinymanager` (Admin)

Online documentation: https://datastorm-open.github.io/shinymanager/

### Available languages

- English
- Français
- Portuguese
- Español
- Deutsch
- Polski
- Japanese
- Chinese
- Indonesian
- Greek

### Password validity period

Using ``options("shinymanager.pwd_validity")``, you can set password validity period. It defaults to ``Inf``. You can specify for example ``options("shinymanager.pwd_validity" = 90)`` if you want to force user changing password each 90 days.

### Failure limit

Using ``options("shinymanager.pwd_failure_limit")``, you can set password failure limit. It defaults to ``Inf``. You can specify for example ``options("shinymanager.pwd_failure_limit" = 5)`` if you want to lock user account after 5 wrong password.


### Cross-application

Adding optional ``applications`` column in *credentials* db: the name of the applications to which the user is authorized, separated by a semicolon. The name of the application corresponds to the name of the directory, or can be declared using: options("shinymanager.application" = "my-app")

### Custom password rules

````
# your own rules
validate_pwd_custom <- function(pwd) {
  all(vapply(
    X = c("[0-9]+", "[a-z]+", "[A-Z]+", "[[:punct:]]+", ".{8,}"),
    FUN = grepl, x = pwd, FUN.VALUE = logical(1)
  ))
}


# server.R
server <- function(input, output, session) {
    
    auth_out <- secure_server(validate_pwd = validate_pwd_custom, ...)
    
    # your classic server logic
    
}
    
````


### Relevant R documentation

````
require(shinymanager)

# Init and using sqlite database
?create_db

# Init and using SQL database
?create_sql_db


# shiny integration
?secure_app
?auth_ui # ui definition


# change labels / language
 ?set_labels

````

### News on shinymanager 1.0.510

* Improve global performance
* Add sparklyr support
* (#188) FEAT: disable write logs and see logs
* (#182 & #187) FIX: quickly bind enter on auth
* FEAT/FIX: SQL features (not only sqlite but PostgreSQL, MySQL, ...)

### News on shinymanager 1.0.500

* (#154) add indonesian. Thanks @aswansyahputra 
* (#178) add chinese. Thanks @wtbxsjy
* FEAT: SQL features (not only sqlite but PostgreSQL, MySQL, ...)

### News on shinymanager 1.0.410

* (#112): fix bug changing user name. Thanks @StatisMike
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

### Installation

Install from CRAN with:

```r
install.packages("shinymanager")
```

Or install development version from GitHub:

```r
remotes::install_github("datastorm-open/shinymanager")
```


### Usage

Secure your Shiny app to control who can access it: 

- ``secure_app()`` & ``auth_ui()`` (customization)
- ``secure_server()`` & ``check_credentials()``

```r
# define some basic credentials (on data.frame)
credentials <- data.frame(
  user = c("shiny", "shinymanager"), # mandatory
  password = c("azerty", "12345"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

library(shiny)
library(shinymanager)

ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output")
)

# Wrap your UI with secure_app
ui <- secure_app(ui)


server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # your classic server logic
  
}

shinyApp(ui, server)
```

Starting page of the application will be:

![](man/figures/shinymanager-login.png)


Once logged in, the application will be launched and a button added to navigate between the app and the admin panel (SQL credentials only and if user is authorized to access it), and to logout from the application:

![](man/figures/shinymanager-info-nav.png)

### Secure Sqlite database

Store your credentials data in Sqlite database protected with a symmetric AES encryption from `openssl`, and password hashing using `scrypt`: 

- ``?create_db``

```r
# Init DB using credentials data
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("azerty", "12345"),
  # password will automatically be hashed
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# you can use keyring package to set database key
library(keyring)
key_set("R-shinymanager-key", "obiwankenobi")

# Init the database
create_db(
  credentials_data = credentials,
  sqlite_path = "path/to/database.sqlite", # will be created
  passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  # passphrase = "passphrase_wihtout_keyring"
)

# Wrap your UI with secure_app, enabled admin mode or not
ui <- secure_app(ui, enable_admin = TRUE)


server <- function(input, output, session) {
  
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
        "path/to/database.sqlite",
        passphrase = key_get("R-shinymanager-key", "obiwankenobi")
        # passphrase = "passphrase_wihtout_keyring"
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # your classic server logic
  ...
}
```

### Use your own SQL database

Store your credentials data in your SQL database using the ``DBI`` interface (and always password hashing using `scrypt`): 

- ``?create_sql_db``

Template available here: https://github.com/datastorm-open/shinymanager/tree/master/inst/sql_config

```r
library(shiny)
library(shinymanager)

#### init the SQL Database
# first edit the .yml configuration file
system.file("sql_config/pg_template.yml", package = "shinymanager")


# Init Credentials data
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("azerty", "12345"), # password will automatically be hashed
  stringsAsFactors = FALSE
)

# Create SQL database
create_sql_db(
  credentials_data = credentials,
  config_path = "path/to/your_sql_configuration.yml"
)

### Use in shiny
ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output")
)

# Wrap your UI with secure_app
ui <- secure_app(ui, choose_language = TRUE)


server <- function(input, output, session) {

 # call the server part
 # check_credentials returns a function to authenticate users
 res_auth <- secure_server(
   check_credentials = check_credentials(db = "path/to/your_sql_configuration.yml")
 )

 # your classic server logic

}

shinyApp(ui, server)
```

### Admin mode

Using SQL/Sqlite database protected, an admin mode is available to manage access to the application, features included are

 * manage users account: add, modify and delete users
 * ask the user to change his password
 * see logs about application usage

![](man/figures/shinymanager-pwd.png)
![](man/figures/shinymanager-admin.png)
![](man/figures/shinymanager-logs.png)

### Use your own function ?

You can also use your own authentification function with ``check_credentials``, for example doing a control to your intern database. ``check_credentials`` must be a function  with two arguments ``user`` & ``password``, returning a ``list`` with at least ``result`` (``TRUE`` to authorize acces, or ``FALSE``) and ``user_info`` (all you want to retrieve from the user in the app):

````R
require(RPostgreSQL)
library(shiny)
library(shinymanager)
library(DBI)
library(glue)

dbname = "*****"
host = "localhost"
port = *****
user = "*****"
password = "******"

con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname , host = host, port = port ,
                 user = user, password = password )


DBI::dbWriteTable(con, "my_table", data.frame(
  user = c("test"),
  password = c("123"),
  stringsAsFactors = FALSE
))

# or a config .yml file or others arguments
my_custom_check_creds <- function(dbname, host, port, db_user, db_password) {
  
 # finally one function of user and password
  function(user, password) {
    
    con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, 
                     host = host, port = port,
                     user = db_user, password = db_password)
    
    on.exit(dbDisconnect(con))
    
    req <- glue_sql("SELECT * FROM my_table WHERE \"user\" = ({user}) AND \"password\" = ({password})", 
             user = user, password = password, .con = con
    )
    
    req <- dbSendQuery(con, req)
    res <- dbFetch(req)
    if (nrow(res) > 0) {
      list(result = TRUE, user_info = list(user = user, something = 123))
    } else {
      list(result = FALSE)
    }
  }
}

ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output")
)
ui <- secure_app(ui)


server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = my_custom_check_creds(
      dbname = "******",
      host = "*****",
      port = ****,
      db_user = "*****",
      db_password = "*******"
    )
  )  
  auth_output <- reactive({
    reactiveValuesToList(res_auth)
  })
  
  # access info
  observe({
    print(auth_output())
  })
}

shinyApp(ui, server)
````

### shiny input

Two inputs are created: 

````
observe({
    print(input$shinymanager_where)
    print(input$shinymanager_language)
})
````

### Customization

You can customize the module (css, image, language, ...).

````
?secure_app
?auth_ui
?set_labels
````

![](man/figures/custom.png)

### Flexdasboard

It's possible to use ``shinymanager`` authentification on ``flexdashboard`` (but not admin console at moment). You can find information on [this discussion](https://github.com/datastorm-open/shinymanager/issues/51). But it's not a really secure way because user can overpass the authentification using developper console... Prefer use  ``shiny`` application with ``secure_app`` function.

### shinyapps.io

There's no persistent data storage on ``shinyapps.io``, you can read more here: https://docs.rstudio.com/shinyapps.io/Storage.html. So your **sqlite** database is lost when the instance is closed, and the one you've pushed when deploying the application will be used. You have to use external database.

### Troubleshooting

The application works fine without ``shinymanager`` but not you have trouble using ``shinymanager``. 

There is a *lag* between your ``ui`` and the ``server``, since ``shinymanger`` hides the ``ui`` part until authentication is successful. It is therefore possible that some of `ui element`` (input) are not defined and are NULL. In this case, you'll see some warning / error message in your R console.

So we recommend to use in all your reactive/observer functions the ``req`` instruction  to **validate the inputs**.

One more *global and brutal* solution can be:

````
server <- function(input, output, session) {
  
  auth_out <- secure_server(....)
  
  observe({
    if(is.null(input$shinymanager_where) || (!is.null(input$shinymanager_where) && input$shinymanager_where %in% "application")){
      
      # your server app code
    }
  })
}
````

But it's better to use ``req`` solution. More discussion on https://github.com/datastorm-open/shinymanager/issues/36

### HTTP request

`shinymanager` use http request and sha256 tokens to grant access to the application, like this the source code is protected without having the need to change your UI or server code.


### About security

The credentials database is secured with a pass phrase and the [`openssl`](https://github.com/jeroen/openssl) package. Hashed password using [`scrypt`](https://github.com/rstudio/rscrypt). If you have concern about method we use, please fill an [issue](https://github.com/datastorm-open/shinymanager/issues).



### Related work

Package [`shinyauthr`](https://github.com/PaulC91/shinyauthr) provides a nice shiny module to add an authentication layer to your shiny apps.



