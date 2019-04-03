
#' @importFrom shiny NS dateInput checkboxInput textInput
#' @importFrom htmltools tagList
#' @importFrom R.utils capitalize
edit_user_UI <- function(id, credentials, username = NULL) {

  ns <- NS(id)

  if (!is.null(username) && username %in% credentials$user) {
    data_user <- credentials[credentials$user == username, ]
  } else {
    data_user <- list()
  }

  tagList(
    lapply(
      X = names(credentials),
      FUN = function(x) {
        if (x %in% "start") {
          value <- data_user[[x]]
          if (is.null(value)) {
            value <- Sys.Date()
          }
          dateInput(inputId = ns(x), label = R.utils::capitalize(x), value = value, width = "100%")
        } else if (x %in% "expire") {
          value <- data_user[[x]]
          if (is.null(value)) {
            value <- Sys.Date() + 60
          }
          dateInput(inputId = ns(x), label = R.utils::capitalize(x), value = value, width = "100%")
        } else if (identical(x, "password")) {
          NULL
        } else if (identical(x, "admin")) {
          checkboxInput(inputId = ns(x), label = R.utils::capitalize(x), value = isTRUE(as.logical(data_user[[x]])))
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
    rv$user <- lapply(
      X = reactiveValuesToList(input),
      FUN = as.character
    )
  })

  return(rv)
}


#' @importFrom utils modifyList
update_user <- function(df, value, username) {
  df <- split(x = df, f = df$user)
  user <- as.list(df[[username]])
  new <-  modifyList(x = user, val = value)
  df[[username]] <- as.data.frame(new)
  do.call(rbind, c(df, list(make.row.names = FALSE)))
}
