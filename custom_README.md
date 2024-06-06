All functions we added are located in R/custom_functions.R.

The key() function returns the user's password. It is defined in R/module-auth.R as follows:
```R
observe({
  if (!is.null(input$user_pwd)) {
    key <<- function() {
      return(input$user_pwd)
    }
  }
})
```

The package works with two databases: keys_database.sqlite and shiny_users.sqlite, both located in base-data/database. In keys_database, sensitive data is stored encrypted with a master key. In shiny_users, users are created, with the master key encrypted for each user using their password and stored in the database.

keys_database has the following columns:
* name*, string
* encrypted_data*, string
* description, string

shiny_users has the following columns:
* user*, string
* password*, string
* start, date
* expire, date
* admin*, boolean
* permission, string
* encrypted_master_key, string
* applications, string
* is_hashed_password, 1

columns marked with * are required

### Build Process

When you have updated anything you need to run `devtools::document()` and `devtools::build()` and then commit and push to the repo.
With `packageVersion("shinymanager")` you can see what version you have installed.
