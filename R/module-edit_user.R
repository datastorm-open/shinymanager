
#' @importFrom shiny NS dateInput checkboxInput textInput
#' @importFrom htmltools taglist
#' @importFrom R.utils capitalize
edit_user_UI <- function(id, data, user = NULL) {
  ns <- NS(id)
  if (!is.null(user) && user %in% data$user) {
    data_user <- data[data$user == user, ]
  } else {
    data_user <- list()
  }
  tagList(
    lapply(
      X = names(data),
      FUN = function(x) {
        if (x %in% c("expire", "start")) {
          dateInput(inputId = ns(x), label = R.utils::capitalize(x), value = data_user[[x]], width = "100%")
        } else if (identical(x, "password")) {
          NULL
        } else if (identical(x, "admin")) {
          checkboxInput(inputId = ns(x), label = R.utils::capitalize(x), value = data_user[[x]])
        } else {
          textInput(inputId = ns(x), label = R.utils::capitalize(x), value = data_user[[x]], width = "100%")
        }
      }
    )
  )
}

#' @importFrom shiny reactiveValues observe reactiveValuesToList
edit_user <- function(input, output, session) {

  rv <- reactiveValues(user = NULL)

  observe({
    rv$user <- reactiveValuesToList(input)
  })

  return(rv)
}

