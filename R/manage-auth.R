
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
#' @importFrom shiny callModule
#'
#' @rdname manage-auth
manage_auth_server <- function(session, check_credentials) {
  callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials,
    use_token = TRUE
  )
}
