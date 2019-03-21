
#' Manage authentication in a Shiny application
#'
#' @param ui UI of the application.
#' @param ... Arguments passed to \code{\link{auth_ui}}.
#'
#' @export
#'
#' @importFrom shiny parseQueryString fluidPage actionButton icon
#' @importFrom htmltools tagList
#'
#' @name manage-auth
#'
manage_auth_app <- function(ui, ...) {
  function(req) {
    token <- parseQueryString(req$QUERY_STRING)$token
    if (validate_token(token)) {
      tagList(
        ui,
        fab_button(
          actionButton(
            inputId = ".shinymanager_logout",
            label = NULL,
            tooltip = "Logout",
            icon = icon("sign-out")
          )
        )
      )
    } else {
      args <- list(...)
      args$id <- "auth"
      fluidPage(
        do.call(auth_ui, args)
      )
    }
  }
}


#' @param session Shiny session
#' @param check_credentials Function passed to \code{\link{auth_server}}.
#'
#' @export
#'
#' @importFrom shiny callModule getQueryString parseQueryString updateQueryString observe
#'
#' @rdname manage-auth
manage_auth_server <- function(session, check_credentials) {

  callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials,
    use_token = TRUE
  )

  user_info <- reactiveValues(user = NULL, user_info = NULL)

  observe({
    query <- getQueryString(session = session)
    token <- query$token
    user <- get_user(token)
    user_info$user <- user
    user_info$user_info <- get_user_info(user)
  })

  observeEvent(session$input$.shinymanager_logout, {
    query <- getQueryString(session = session)
    token <- query$token
    remove_token(token)
    updateQueryString(queryString = "?", session = session, mode = "replace")
    session$reload()
  })

  return(user_info)
}
