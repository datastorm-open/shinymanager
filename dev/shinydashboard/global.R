

# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)
library(shinydashboard)


# data.frame with credentials info
credentials <- data.frame(
  user = c("fanny", "victor", "benoit"),
  password = c("azerty", "12345", "azerty"),
  comment = c("alsace", "auvergne", "bretagne"),
  stringsAsFactors = FALSE
)
