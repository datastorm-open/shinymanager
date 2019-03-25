
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
    query <- parseQueryString(req$QUERY_STRING)
    token <- query$token
    admin <- query$admin
    if (validate_token(token)) {
      if (is_token_admin(token) & identical(admin, "true")) {
        fluidPage(
          tags$h2("Welcome to admin mode!")
        )
      } else {
        if (is_token_admin(token)) {
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
    if (!is.null(token)) {
      user <- get_user(token)
      user_info$user <- user
      user_info$user_info <- get_user_info(user)
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
    remove_token(token)
    updateQueryString(queryString = "?", session = session, mode = "replace")
    session$reload()
  })

  return(user_info)
}
