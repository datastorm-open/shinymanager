# shinymanager <img src="inst/demo_app/www/figures/shinymanager.png" width=200 align="right" />

[![Travis build status](https://travis-ci.org/datastorm-open/shinymanager.svg?branch=master)](https://travis-ci.org/datastorm-open/shinymanager)
[![version](http://www.r-pkg.org/badges/version/shinymanager)](https://CRAN.R-project.org/package=shinymanager)
[![cranlogs](http://cranlogs.r-pkg.org/badges/shinymanager)](https://CRAN.R-project.org/package=shinymanager)
[![cran checks](https://cranchecks.info/badges/worst/shinymanager)](https://cranchecks.info/pkgs/shinymanager)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

> Simple and secure authentication mechanism for single 'Shiny' applications. Credentials are stored in an encrypted 'SQLite' database. Password are hashed using 'scrypt'  R package. Source code of main application is protected until authentication is successful.


Live demo:

* On Shiny-server: http://shinyapps.dreamrs.fr/shinymanager-demo/
* On shinyapps.io : https://dreamrs.shinyapps.io/shinymanager-demo/

You can authenticate with:
 * user: `shiny` / password: `shiny`
 * user: `shinymanager` / password: `shinymanager` (Admin)

Online documentation : https://datastorm-open.github.io/shinymanager/

### News on shinymanager 1.0.400

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

### Installation

Install from CRAN with :

```r
install.packages("shinymanager")
```

Or install development version from GitHub :

```r
remotes::install_github("datastorm-open/shinymanager")
```


### Usage

Secure your Shiny app to control who can access it : 

- ``secure_app()`` & ``auth_ui()`` (customization)
- ``secure_server()`` & ``check_credentials()``

```r
# define some credentials
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

Starting page of the application will be :

![](inst/demo_app/www/figures/shinymanager-login.png)


Once logged, the application will be launched and a button added to navigate between the app and the admin panel (SQL credentials only and if user is authorized to access it), and to logout from the application :

![](inst/demo_app/www/figures/shinymanager-info-nav.png)

### Secure database

Store your credentials data in SQL database protected with a symmetric AES encryption from `openssl`, and password hashing using `scrypt` : 

- ``create_db()``

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

### Admin mode

Using SQL database protected, an admin mode is available to manage access to the application, features included are

 * manage users account : add, modify and delete users
 * ask the user to change his password
 * see logs about application usage

![](inst/demo_app/www/figures/shinymanager-pwd.png)
![](inst/demo_app/www/figures/shinymanager-admin.png)
![](inst/demo_app/www/figures/shinymanager-logs.png)

### Use your own function ?

You can also use your own authentification function with ``check_credentials``, for example doiing a control to your intern database. ``check_credentials`` must be a function  with two arguments ``user`` & ``password``, returning a least with at least ``result`` (``TRUE`` to authorize acces, or ``FALSE``) and ``user_info`` (all you want to retrieve from the user in the app) :

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

Two inputs are created : 

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

![](inst/demo_app/www/figures/custom.png)

### Flexdasboard

It's possible to use ``shinymanager`` authentification on ``flexdashboard`` (but not admin console at moment). You can find information on [this discussion](https://github.com/datastorm-open/shinymanager/issues/51). But it's not a really secure way because user can overpass the authentification using developper console... Prefer use  ``shiny`` application with ``secure_app`` function.

### shinyapps.io

There's no persistent data storage on ``shinyapps.io``, you can read more here : https://docs.rstudio.com/shinyapps.io/Storage.html. So your **sqlite** database is lost when the instance is closed, and the one you've pushed when deploying the application will be used. You have to use external database.

It's possible to use ``shinymanager`` authentification on ``flexdashboard`` (but not admin console at moment). You can find information on [this discussion](https://github.com/datastorm-open/shinymanager/issues/51). But it's not a really secure way because user can overpass the authentification using developper console... Prefer use  ``shiny`` application with ``secure_app`` function.

### Troubleshooting

The application works fine without ``shinymanager`` but not you have trouble using ``shinymanager``. 

There is a *lag* between your ``ui`` and the ``server``, since ``shinymanger`` hides the ``ui`` part until authentication is successful. It is therefore possible that some of `ui element`` (input) are not defined and are NULL. In this case, you'll see some warning / error message in your R console.

So we recommend to use in all your reactive/observer functions the ``req`` instruction  to **validate the inputs**.

One more *global and brutal* solution can be :

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



