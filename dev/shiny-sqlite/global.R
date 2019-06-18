

# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)


# data.frame with credentials info
credentials <- data.frame(
  user = c("benoit.thieurmel@datastorm.fr", "victor", "benoit"),
  password = c("azerty", "12345", "azerty"),
  comment = c("alsace", "auvergne", "bretagne"),
  applications = c("app1;shiny-sqlite", "app1", "shiny-sqlite"),
  age = c(14, 20, 30),
  expire = as.Date(c(NA, "2019-12-31", "2019-12-31")),
  admin = c(TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

# if (!file.exists("credentials.sqlite")) {
  create_db(credentials_data = credentials, sqlite_path = "credentials.sqlite", passphrase = "supersecret")
# }
