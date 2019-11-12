

# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)


# data.frame with credentials info
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("shiny", "shinymanager"),
  comment = c("Easy interactive web applications with R",
              "Simple and secure authentification mechanism for single 'Shiny' applications."),
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Create credentials DB (only once)
# create_db(
#   credentials_data = credentials,
#   sqlite_path = "examples/demo/credentials.sqlite",
#   passphrase = "supersecret"
# )

