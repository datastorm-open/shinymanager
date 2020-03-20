
#' Enable / Disable a Button
#'
#' @param session shiny session.
#' @param inputId Input's id to enable / disable.
#' @param type 'enable' or 'disable'.
#'
#' @noRd
toggleBtn <- function(session, inputId, type = "disable") {
  session$sendCustomMessage(
    type = "togglewidget",
    message = list(inputId = inputId, type = type)
  )
}


# Send input value to know where we are (what ui is displayed: auth, admin, app, ...)

shinymanager_where <- function(where) {
  # tags$script(sprintf(
  #   paste0("$(document).ready(function(){Shiny.setInputValue('shinymanager_where', '", where, "');})")
  # ))
  # humm, little hack ^^
  tags$div(
    style = "display: none;",
    selectInput(inputId = "shinymanager_where", label = NULL, 
               choices = where, selected = where, multiple = TRUE)
  )
}

shinymanager_language <- function(lan) {
  tags$div(
    style = "display: none;",
    selectInput(inputId = "shinymanager_language", label = NULL, 
                choices = lan, selected = lan, multiple = TRUE)
  )
}


# Remove the whole query string
#' @importFrom shiny updateQueryString getDefaultReactiveDomain
clearQueryString <- function(session = getDefaultReactiveDomain()) {
  updateQueryString(session$clientData$url_pathname, mode = "replace", session = session)
}

# Retrieve token from the query string
#' @importFrom shiny getQueryString getDefaultReactiveDomain
getToken <- function(session = getDefaultReactiveDomain()) {
  query <- getQueryString(session = session)
  query$token
}

# Remove the token from the query string
#' @importFrom shiny updateQueryString getQueryString getDefaultReactiveDomain
resetQueryString <- function(session = getDefaultReactiveDomain()) {
  query <- getQueryString(session = session)
  query$token <- NULL
  if (length(query) == 0) {
    clearQueryString(session = session)
  } else {
    query <- paste(names(query), query, sep = "=", collapse="&")
    updateQueryString(queryString = paste0("?", query), mode = "replace", session = session)
  }
}



#' @importFrom htmltools tags doRenderTags
#' @importFrom shiny icon
input_btns <- function(inputId, users, tooltip, icon, status = "primary") {
  lan <- use_language()
  tag <- lapply(
    X = users,
    FUN = function(x) {
      res <- tags$button(
        class = paste0("btn btn-", status),
        style = "float: right;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s',  {priority: 'event'})",
          inputId, x
        ),
        icon,
        `data-toggle` = "tooltip",
        `data-title` = lan$get(tooltip),
        `data-container` = "body"
      )
      res <- tagList(res, tags$script(HTML("$('[data-toggle=\"tooltip\"]').tooltip();")))
      doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}


input_checkbox_ui <- function(id, users, checked = FALSE) {
  ns <- NS(id)
  tag <- lapply(
    X = users,
    FUN = function(x) {
      # res <- checkboxInput(inputId = ns(paste0("check_", x)), label = NULL, value = FALSE)
      res <- tags$input(id = ns(paste0("check_", x)), type = "checkbox", style = "float: right;")
      if(checked) res$attribs$checked <- "checked"
      doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}

input_checkbox <- function(input, output, session) {
  
  reac <- reactive({
    inputs <- reactiveValuesToList(input)
    inputs <- dropFalse(inputs)
    gsub(pattern = "^check_", replacement = "", x = names(inputs))
  })
  
  return(reac)
}

unbindDT <- function(id, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "unbindDT",
    message = list(id = id)
  )
}
