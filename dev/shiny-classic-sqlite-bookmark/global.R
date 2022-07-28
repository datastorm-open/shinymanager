

# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)


# data.frame with credentials info
credentials <- data.frame(
  user = c("fanny", "victor", "benoit"),
  password = c("azerty", "12345", "azerty"),
  comment = c("alsace", "auvergne", "bretagne"),
  admin = c(FALSE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

# if (!file.exists("credentials.sqlite")) {
  # create_db(credentials_data = credentials, sqlite_path = "dev/shiny-classic-sqlite/credentials.sqlite", passphrase = "supersecret")
# }

#conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = "dev/shiny-classic-sqlite/credentials.sqlite")
#read_db_decrypt(conn, passphrase = "supersecret")
#DBI::dbDisconnect(conn)
