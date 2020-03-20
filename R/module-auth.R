
#' Authentication module
#'
#' @param id Module's id.
#' @param status Bootstrap status to use for the panel and the button.
#'  Valid status are: \code{"default"}, \code{"primary"}, \code{"success"},
#'  \code{"warning"}, \code{"danger"}.
#' @param tags_top A \code{tags (div, img, ...)} to be displayed on top of the authentication module.
#' @param tags_bottom A \code{tags (div, img, ...)} to be displayed on bottom of the authentication module.
#' @param background A optionnal \code{css} for authentication background. See example.
#' @param choose_language \code{logical/character}. Add language selection on top ? TRUE for all supported languages
#' or a vector of possibilities like \code{c("fr", "en")}. If enabled, \code{input$shinymanager_language} is created
#' @param ... : Used for old version compatibility.
#' 
#' 
#' @export
#'
#' @name module-authentication
#'
#' @importFrom htmltools tagList tags singleton
#' @importFrom shiny NS fluidRow column textInput passwordInput actionButton uiOutput
#'
#' @example examples/module-auth.R
auth_ui <- function(id, status = "primary", tags_top = NULL, 
                    tags_bottom = NULL, background = NULL, 
                    choose_language = NULL, ...) {

  ns <- NS(id)

  lan <- use_language()

  # patch / message changing tag_img & tag_div
  deprecated <- list(...)
  if("tag_img" %in% names(deprecated)){
    tags_top <- deprecated$tag_img
    warning("'tag_img' (auth_ui, secure_app) is now deprecated. Please use 'tags_top'", call. = FALSE)
  }
  if("tag_div" %in% names(deprecated)){
    tags_bottom <- deprecated$tag_div
    warning("'tag_div' (auth_ui, secure_app) is now deprecated. Please use 'tags_bottom'", call. = FALSE)
  }
  tagList(
    singleton(tags$head(
      tags$link(href="shinymanager/styles-auth.css", rel="stylesheet"),
      tags$script(src = "shinymanager/bindEnter.js"),
      if(!is.null(background)){
        background <- gsub(";$", "", background)
        tags$style(HTML(paste0(".panel-auth {background:", background, ",#FFF;}")))
      }
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
              if (!is.null(choose_language)){
                choices = NULL
                if(is.logical(choose_language) && choose_language){
                  choices = lan$get_language_registered()
                } else if(is.character(choose_language)){
                  choices = unique(c(intersect(choose_language, lan$get_language_registered()), lan$get_language()))
                }
                
                if(length(choices) > 1){
                  selected = ifelse(lan$get_language() %in% choices, 
                                    lan$get_language(),
                                    choices[1])
                  
                  tags$div(
                    style = "text-align: left; font-size: 12px;",
                    selectInput(
                      inputId = ns("language"),
                      label = NULL,
                      choices = choices,
                      selected = selected,
                      width = "20%"
                    )
                  )
                }
              },
              tags$div(
                style = "text-align: center;",
                if (!is.null(tags_top)) tags_top,
                uiOutput(ns("auth_title"))
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
              if (!is.null(tags_bottom)) tags$hr(), tags_bottom,
              uiOutput(ns("update_shinymanager_language"))
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
#' @importFrom shiny reactiveValues observeEvent removeUI updateQueryString insertUI icon updateActionButton updateTextInput renderUI
#' @importFrom stats setNames
auth_server <- function(input, output, session, check_credentials, use_token = FALSE) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  observe({
    session$sendCustomMessage(
      type = "focus_input",
      message = list(inputId = ns("user_id"))
    )
  })
  
  lan <- use_language()

  auth_title <- reactiveVal(lan$get("Please authenticate"))
  observe({
    if(!is.null(input$language)){
      lan$set_language(input$language) 
      updateTextInput(session, inputId = "user_id", label = lan$get("Username:"))
      updateTextInput(session, inputId = "user_pwd", label = lan$get("Password:"))
      updateActionButton(session, inputId = "go_auth", label = lan$get("Login"))

      auth_title(lan$get("Please authenticate"))
      
      output$update_shinymanager_language <- renderUI({
        shinymanager_language(lan$get_language())
      })
    }
  })
  
  output$auth_title <- renderUI({
    tags$h3(auth_title())
  })
  
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




