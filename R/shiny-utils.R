
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



# Remove the whole query string
#' @importFrom shiny updateQueryString getDefaultReactiveDomain
clearQueryString <- function(session = getDefaultReactiveDomain()) {
  updateQueryString("/", mode = "replace", session = session)
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
    updateQueryString(queryString = "/", mode = "replace", session = session)
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





