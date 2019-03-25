

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

# Function to authenticate user
check_credentials_p <- purrr::partial(
  check_credentials_df,
  credentials_df = credentials # set default df to use
)

