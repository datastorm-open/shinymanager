
#' Manage authentication in a Shiny application
#'
#' @param ui UI of the application.
#' @param ... Arguments passed to \code{\link{auth_ui}}.
#' @param head_auth Tag or list of tags to use in the \code{<head>}
#'  of the authentication page (for custom CSS for example).
#'
#' @export
#'
#' @importFrom shiny parseQueryString fluidPage actionButton icon
#' @importFrom htmltools tagList
#'
#' @name manage-auth
#'
manage_auth_app <- function(ui, ..., head_auth = NULL) {
  function(req) {
    query <- parseQueryString(req$QUERY_STRING)
    token <- query$token
    admin <- query$admin
    if (.tok$is_valid(token)) {
      if (.tok$is_admin(token) & identical(admin, "true")) {
        fluidPage(
          tags$h2("Welcome to admin mode!")
        )
      } else {
        if (.tok$is_admin(token)) {
          menu <- fab_button(
            actionButton(
              inputId = ".shinymanager_logout",
              label = NULL,
              tooltip = "Logout",
              icon = icon("sign-out")
            ),
            actionButton(
              inputId = ".shinymanager_admin",
              label = NULL,
              tooltip = "Admin mode",
              icon = icon("cogs")
            )
          )
        } else {
          menu <- fab_button(
            actionButton(
              inputId = ".shinymanager_logout",
              label = NULL,
              tooltip = "Logout",
              icon = icon("sign-out")
            )
          )
        }

        tagList(ui, menu)
      }
    } else {
      args <- list(...)
      args$id <- "auth"
      fluidPage(
        tags$head(head_auth),
        do.call(auth_ui, args)
      )
    }
  }
}


#' @param check_credentials Function passed to \code{\link{auth_server}}.
#' @param session Shiny session.
#'
#' @export
#'
#' @importFrom shiny callModule getQueryString parseQueryString updateQueryString observe getDefaultReactiveDomain
#'
#' @rdname manage-auth
manage_auth_server <- function(check_credentials, session = shiny::getDefaultReactiveDomain()) {

  callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials,
    use_token = TRUE
  )

  user_info_rv <- reactiveValues()

  observe({
    query <- getQueryString(session = session)
    token <- query$token
    if (!is.null(token)) {
      user_info <- .tok$get(token)
      for (i in names(user_info)) {
        user_info_rv[[i]] <- user_info[[i]]
      }
    }
  })

  observeEvent(session$input$.shinymanager_admin, {
    query <- getQueryString(session = session)
    token <- query$token
    updateQueryString(queryString = sprintf("?token=%s&admin=true", token), session = session, mode = "replace")
    session$reload()
  })

  observeEvent(session$input$.shinymanager_logout, {
    query <- getQueryString(session = session)
    token <- query$token
    .tok$remove(token)
    updateQueryString(queryString = "?", session = session, mode = "replace")
    session$reload()
  })

  return(user_info_rv)
}
