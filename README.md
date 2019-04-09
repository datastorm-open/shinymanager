# shinymanager

[![Travis build status](https://travis-ci.org/datastorm-open/shinymanager.svg?branch=master)](https://travis-ci.org/datastorm-open/shinymanager)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

> Simple and secure authentification mechanism for single 'Shiny' applications. Credentials are stored in an encrypted 'SQLite' database. Source code of main application is protected until authentication is successful.


### Installation

```r
remotes::install_github("datastorm-open/shinymanager")
```


### Usage

Secure your Shiny app to control who can access it : 

```r
# define some credentials
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("azerty", "12345"),
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

### HTTP request

`shinymanager` use http request and sha256 tokens to grant acces to the application, like this the source code is protected without having the need to change your UI or server code.


### Secure database

Store your credentials data in SQL database protected with a symmetric AES encryption from `openssl` : 


```r
# Credentials data
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("azerty", "12345"),
  stringsAsFactors = FALSE
)

# you can use keyring package to set database key
library(keyring)
key_set("R-shinymanager-key", "obiwankenobi")

# Create the database
create_db(
  credentials_data = credentials,
  sqlite_path = "path/to/database.sqlite", # will be created
  passphrase = key_get("R-shinymanager-key", "obiwankenobi")
)
```

### Admin mode

An admin mode is available to manage access to the application.
