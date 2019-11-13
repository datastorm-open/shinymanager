
#' Authentication module
#'
#' @param id Module's id.
#' @param status Bootstrap status to use for the panel and the button.
#'  Valid status are: \code{"default"}, \code{"primary"}, \code{"success"},
#'  \code{"warning"}, \code{"danger"}.
#' @param tag_img A \code{tags$img} to be displayed on top of the authentication module.
#' @param tag_div A \code{tags$div} to be displayed on bottom of the authentication module.
#'
#' @export
#'
#' @name module-authentication
#'
#' @importFrom htmltools tagList tags singleton
#' @importFrom shiny NS fluidRow column textInput passwordInput actionButton
#'
#' @example examples/module-auth.R
auth_ui <- function(id, status = "primary", tag_img = NULL, tag_div = NULL) {

  ns <- NS(id)

  lan <- use_language()

  tagList(
    singleton(tags$head(
      tags$link(href="shinymanager/styles-auth.css", rel="stylesheet"),
      tags$script(src = "shinymanager/bindEnter.js")
    )),
    tags$div(
      id = ns("auth-mod"), class = "panel-auth",
      tags$br(), tags$div(style = "height: 70px;"), tags$br(),
      fluidRow(
        column(
          width = 4, offset = 4,
          tags$div(
            class = paste0("panel panel-", status),
            tags$div(
              class = "panel-body",
              tags$div(
                style = "text-align: center;",
                if (!is.null(tag_img)) tag_img,
                tags$h3(lan$get("Please authenticate"))
              ),
              tags$br(),
              textInput(
                inputId = ns("user_id"),
                label = lan$get("Username:"),
                width = "100%"
              ),
              passwordInput(
                inputId = ns("user_pwd"),
                label = lan$get("Password:"),
                width = "100%"
              ),
              tags$br(),
              actionButton(
                inputId = ns("go_auth"),
                label = lan$get("Login"),
                width = "100%",
                class = paste0("btn-", status)
              ),
              tags$br(), tags$br(),
              tags$script(
                sprintf("bindEnter('%s');", ns(""))
              ),
              tags$div(id = ns("result_auth")),
              if (!is.null(tag_div)) tags$hr(), tag_div
            )
          )
        )
      )
    )
  )
}



#' @param input,output,session Standard Shiny server arguments.
#' @param check_credentials Function with two arguments (\code{user},
#'  the username provided by the user and \code{password}, his/her password).
#'  Must return \code{TRUE} or \code{FALSE}.
#'  To use additionnals arguments, set them with \code{purrr::partial} (see examples).
#' @param use_token Add a token in the URL to check authentication. Should not be used directly.
#'
#' @export
#'
#' @rdname module-authentication
#'
#' @return A \code{reactiveValues} with 3 slots :
#'  \itemize{
#'   \item \strong{result} : logical, result of authentication.
#'   \item \strong{user} : character, name of connected user.
#'   \item \strong{user_info} : information about the user.
#'  }
#'
#' @importFrom htmltools tags
#' @importFrom shiny reactiveValues observeEvent removeUI updateQueryString insertUI icon
#' @importFrom stats setNames
auth_server <- function(input, output, session, check_credentials, use_token = FALSE) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  lan <- use_language()

  authentication <- reactiveValues(result = FALSE, user = NULL, user_info = NULL)

  observeEvent(input$go_auth, {
    removeUI(selector = jns("msg_auth"))
    res_auth <- check_credentials(input$user_id, input$user_pwd)
    if (isTRUE(res_auth$result)) {
      removeUI(selector = jns("auth-mod"))
      authentication$result <- TRUE
      authentication$user <- input$user_id
      authentication$user_info <- res_auth$user_info
      # token <- generate_token(input$user_id)
      token <- .tok$generate(input$user_id)

      if (isTRUE(use_token)) {
        # add_token(token, as.list(res_auth$user_info))
        .tok$add(token, as.list(res_auth$user_info))
        updateQueryString(queryString = paste0("?token=", token), session = session)
        session$reload()
      }

    } else {
      if (is.null(res_auth$user_info)) {
        save_logs_failed(input$user_id, status = "Unknown user")
        insertUI(
          selector = jns("result_auth"),
          ui = tags$div(
            id = ns("msg_auth"), class = "alert alert-danger",
            icon("exclamation-triangle"), lan$get("Username or password are incorrect")
          )
        )
      } else if (isTRUE(res_auth$expired)) {
        save_logs_failed(input$user_id, status = "Expired")
        insertUI(
          selector = jns("result_auth"),
          ui = tags$div(
            id = ns("msg_auth"), class = "alert alert-danger",
            icon("exclamation-triangle"), lan$get("Your account has expired")
          )
        )
      } else {
        if (!isTRUE(res_auth$authorized)) {
          save_logs_failed(input$user_id, status = "Unauthorized")
          insertUI(
            selector = jns("result_auth"),
            ui = tags$div(
              id = ns("msg_auth"), class = "alert alert-danger",
              icon("exclamation-triangle"), lan$get("You are not authorized for this application")
            )
          )
        } else {
          save_logs_failed(input$user_id, status = "Wrong pwd")
          insertUI(
            selector = jns("result_auth"),
            ui = tags$div(
              id = ns("msg_auth"), class = "alert alert-danger",
              icon("exclamation-triangle"), lan$get("Username or password are incorrect")
            )
          )
        }
      }
    }
  }, ignoreInit = TRUE)

  return(authentication)
}




