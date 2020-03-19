

# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)


# data.frame with credentials info
credentials <- data.frame(
  user = c("benoit"),
  password = c("azerty"),
  group = NA,
  apps = NA,
  admin = c(TRUE),
  stringsAsFactors = FALSE
)

# if (!file.exists("credentials.sqlite")) {
  create_db(credentials_data = credentials, sqlite_path = "credentials.sqlite", passphrase = "supersecret")
# }
