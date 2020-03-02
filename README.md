# shinymanager <img src="man/figures/shinymanager.png" width=200 align="right" />

[![Travis build status](https://travis-ci.org/datastorm-open/shinymanager.svg?branch=master)](https://travis-ci.org/datastorm-open/shinymanager)
[![version](http://www.r-pkg.org/badges/version/shinymanager)](https://CRAN.R-project.org/package=shinymanager)
[![cranlogs](http://cranlogs.r-pkg.org/badges/shinymanager)](https://CRAN.R-project.org/package=shinymanager)
[![cran checks](https://cranchecks.info/badges/worst/shinymanager)](https://cranchecks.info/pkgs/shinymanager)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

> Simple and secure authentification mechanism for single 'Shiny' applications. Credentials are stored in an encrypted 'SQLite' database. Source code of main application is protected until authentication is successful.


Live demo:

* On Shiny-server: http://shinyapps.dreamrs.fr/shinymanager-demo/
* On shinyapps.io : https://dreamrs.shinyapps.io/shinymanager-demo/

You can authenticate with:
 * user: `shiny` / password: `shiny`
 * user: `shinymanager` / password: `shinymanager` (Admin)



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

![](man/figures/shinymanager-login.png)


Once logged, the application will be launched and a button added to navigate between the app and the admin panel (SQL credentials only and if user is authorized to access it), and to logout from the application :

![](man/figures/shinymanager-info-nav.png)

### Secure database

Store your credentials data in SQL database protected with a symmetric AES encryption from `openssl` : 

- ``create_db()``

```r
# Credentials data
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("azerty", "12345"),
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
)
```

### Admin mode

Using SQL database protected, an admin mode is available to manage access to the application, features included are

 * manage users account : add, modify and delete users
 * ask the user to change his password
 * see logs about application usage

![](man/figures/shinymanager-pwd.png)
![](man/figures/shinymanager-admin.png)
![](man/figures/shinymanager-logs.png)


### HTTP request

`shinymanager` use http request and sha256 tokens to grant access to the application, like this the source code is protected without having the need to change your UI or server code.


### About security

The credentials database is secured with a pass phrase and the [`openssl`](https://github.com/jeroen/openssl) package. If you have concern about method we use, please fill an [issue](https://github.com/datastorm-open/shinymanager/issues).



### Related work

Package [`shinyauthr`](https://github.com/PaulC91/shinyauthr) provides a nice shiny module to add an authentication layer to your shiny apps.



