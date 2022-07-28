require(shiny)
require(shinymanager)
# require(markdown) # rsconnect

# Initialisation de la base via un data.frame
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("shiny", "shinymanager"),
  # password will automatically be hashed
  admin = c(FALSE, TRUE), # utilisateurs avec droits d'admin ?
  stringsAsFactors = FALSE
)


# Initialisation de la base de données
create_db(
  credentials_data = credentials,
  sqlite_path = "database.sqlite", # elle sera crée
  passphrase = "passphrase_wihtout_keyring"
)
