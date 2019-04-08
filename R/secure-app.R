
#' Secure a Shiny application and manage authentication
#'
#' @param ui UI of the application.
#' @param ... Arguments passed to \code{\link{auth_ui}}.
#' @param enable_admin Enable or not access to admin mode, note that
#'  admin mode is only available when using SQLite backend for credentials.
#' @param head_auth Tag or list of tags to use in the \code{<head>}
#'  of the authentication page (for custom CSS for example).
#'
#' @export
#'
#' @importFrom shiny parseQueryString fluidPage actionButton icon
#' @importFrom htmltools tagList
#'
#' @name secure-app
#'
#' @examples
#' if (interactive()) {
#'
#'   # define some credentials
#'   credentials <- data.frame(
#'     user = c("shiny", "shinymanager"),
#'     password = c("azerty", "12345"),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   library(shiny)
#'   library(shinymanager)
#'
#'   ui <- fluidPage(
#'     tags$h2("My secure application"),
#'     verbatimTextOutput("auth_output")
#'   )
#'
#'   # Wrap your UI with secure_app
#'   ui <- secure_app(ui)
#'
#'
#'   server <- function(input, output, session) {
#'
#'     # call the server part
#'     # check_credentials returns a function to authenticate users
#'     res_auth <- secure_server(
#'       check_credentials = check_credentials(credentials)
#'     )
#'
#'     output$auth_output <- renderPrint({
#'       reactiveValuesToList(res_auth)
#'     })
#'
#'     # your classic server logic
#'
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
secure_app <- function(ui, ..., enable_admin = FALSE, head_auth = NULL) {
  lan <- use_language()
  ui <- force(ui)
  enable_admin <- force(enable_admin)
  head_auth <- force(head_auth)
  function(request) {
    query <- parseQueryString(request$QUERY_STRING)
    token <- query$token
    admin <- query$admin
    # browser()
    if (.tok$is_valid(token)) {
      if (isTRUE(enable_admin) && .tok$is_admin(token) & identical(admin, "true")) {
        fluidPage(
          admin_UI("admin"),
          fab_button(
            actionButton(
              inputId = ".shinymanager_logout",
              label = NULL,
              tooltip = lan$get("Logout"),
              icon = icon("sign-out")
            ),
            actionButton(
              inputId = ".shinymanager_app",
              label = NULL,
              tooltip = lan$get("Go to application"),
              icon = icon("share")
            )
          )
        )
      } else {
        if (isTRUE(enable_admin) && .tok$is_admin(token)) {
          menu <- fab_button(
            actionButton(
              inputId = ".shinymanager_logout",
              label = NULL,
              tooltip = lan$get("Logout"),
              icon = icon("sign-out")
            ),
            actionButton(
              inputId = ".shinymanager_admin",
              label = NULL,
              tooltip = lan$get("Administrator mode"),
              icon = icon("cogs")
            )
          )
        } else {
          menu <- fab_button(
            actionButton(
              inputId = ".shinymanager_logout",
              label = NULL,
              tooltip = lan$get("Logout"),
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
#' @importFrom shiny callModule getQueryString parseQueryString updateQueryString observe getDefaultReactiveDomain isolate
#'
#' @rdname secure-app
secure_server <- function(check_credentials, session = shiny::getDefaultReactiveDomain()) {

  isolate(resetQueryString(session = session))

  callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials,
    use_token = TRUE
  )

  path_sqlite <- .tok$get_sqlite_path()
  if (!is.null(path_sqlite)) {
    callModule(
      module = admin,
      id = "admin",
      sqlite_path = path_sqlite,
      passphrase = .tok$get_passphrase()
    )
  }

  user_info_rv <- reactiveValues()

  observe({
    token <- getToken(session = session)
    if (!is.null(token)) {
      user_info <- .tok$get(token)
      for (i in names(user_info)) {
        user_info_rv[[i]] <- user_info[[i]]
      }
    }
  })

  observeEvent(session$input$.shinymanager_admin, {
    token <- getToken(session = session)
    updateQueryString(queryString = sprintf("?token=%s&admin=true", token), session = session, mode = "replace")
    .tok$reset_count(token)
    session$reload()
  })

  observeEvent(session$input$.shinymanager_app, {
    token <- getToken(session = session)
    updateQueryString(queryString = sprintf("?token=%s", token), session = session, mode = "replace")
    .tok$reset_count(token)
    session$reload()
  })

  observeEvent(session$input$.shinymanager_logout, {
    token <- getToken(session = session)
    .tok$remove(token)
    clearQueryString(session = session)
    session$reload()
  })

  return(user_info_rv)
}


