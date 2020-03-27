

# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)


# data.frame with credentials info
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("shiny", "shinymanager"),
  group = c("all", NA),
  comment = c("Easy interactive web applications with R",
              "Simple and secure authentification mechanism for single 'Shiny' applications."),
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# # Create credentials DB (only once)
# create_db(
#   credentials_data = credentials,
#   sqlite_path = "credentials.sqlite",
#   passphrase = "supersecret"
# )

set_labels(
  language = "en",
  "Please authenticate" = "You have to login",
  "Username:" = "What's your name:",
  "Password:" = "Enter your password:"
)
