``` r
require(shinymanager)

credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("shiny", "shinymanager"),
  # password will automatically be hashed
  admin = c(FALSE, TRUE), # utilisateurs avec droits d'admin ?
  stringsAsFactors = FALSE
)

create_db(
  credentials_data = credentials,
  sqlite_path = "database.sqlite", # elle sera crée
  passphrase = "passphrase_wihtout_keyring"
)
```
