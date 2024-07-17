#' Adds the content of inst/assets/ to shinymanager/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("shinymanager", system.file("assets", package = "shinymanager"))
}

shinymanager_con <- new.env(hash=TRUE)
