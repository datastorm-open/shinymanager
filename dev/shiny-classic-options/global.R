

# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)


# data.frame with credentials info
credentials <- data.frame(
  user = c("fanny", "victor", "benoit"),
  password = c("azerty", "12345", "azerty"),
  comment = c("alsace", "auvergne", "bretagne"),
  admin = "TRUE",
  stringsAsFactors = FALSE
)

if (!file.exists("credentials.sqlite")) {
  create_db(credentials_data = credentials, sqlite_path = "credentials.sqlite", passphrase = "supersecret")
}
