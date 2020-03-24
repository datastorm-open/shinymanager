

# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)


# data.frame with credentials info
credentials <- data.frame(
  user = c("fanny", "victor", "benoit"),
  password = c("azerty", "12345", "azerty"),
  # comment = c("alsace", "auvergne", "bretagne"),
  stringsAsFactors = FALSE
)

# Customize some labels displayed in application
set_labels(
  "Please authenticate" = "You have to login!",
  "Username:" = "What's your name:",
  "Password:" = "Enter your password:"
)

