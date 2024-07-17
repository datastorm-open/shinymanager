
#' New password module
#'
#' @param id Module's id.
#' @param tag_img A \code{tags$img} to be displayed on the authentication module.
#' @param status Bootstrap status to use for the panel and the button.
#'  Valid status are: \code{"default"}, \code{"primary"}, \code{"success"},
#'  \code{"warning"}, \code{"danger"}.
#'
#' @export
#'
#' @name module-password
#'
#' @importFrom htmltools tagList singleton tags
#' @importFrom shiny NS fluidRow column passwordInput actionButton
#'
#' @example examples/module-pwd.R
pwd_ui <- function(id, tag_img = NULL, status = "primary", lan = NULL) {
  
  ns <- NS(id)
  
  if(is.null(lan)){
    lan <- use_language()
  }
  
  tagList(
    singleton(tags$head(
      tags$link(href="shinymanager/styles-auth.css", rel="stylesheet"),
      tags$script(src = "shinymanager/bindEnter.js")
    )),
    tags$div(
      id = ns("pwd-mod"), class = "panel-auth",
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
                tags$h3(lan$get("Please change your password"))
              ),
              tags$br(),
              passwordInput(
                inputId = ns("pwd_one"),
                label = lan$get("New password:"),
                width = "100%"
              ),
              passwordInput(
                inputId = ns("pwd_two"),
                label = lan$get("Confirm password:"),
                width = "100%"
              ),
              tags$span(
                class = "help-block",
                icon("circle-info"),
                lan$get("Password must contain at least one number, one lowercase, one uppercase and must be at least length 6.")
              ),
              tags$br(),
              tags$div(
                id = ns("container-btn-update"),
                actionButton(
                  inputId = ns("update_pwd"),
                  label = lan$get("Update new password"),
                  width = "100%",
                  class = paste0("btn-", status)
                ),
                tags$br(), tags$br()
              ),
              tags$script(
                sprintf("bindEnter('%s');", ns(""))
              ),
              tags$br(),
              tags$div(id = ns("result_pwd"))
            )
          )
        )
      )
    )
  )
}

#' @param input,output,session Standard Shiny server arguments.
#' @param user A \code{reactiveValues} with a slot \code{user},
#'  referring to the user for whom the password is to be changed.
#' @param update_pwd A \code{function} to perform an action when changing password is successful.
#'  Two arguments will be passed to the function: \code{user} (username) and \code{password}
#'  (the new password). Must return a list with at least a slot \code{result} with \code{TRUE}
#'  or \code{FALSE}, according if the update has been successful.
#' @param validate_pwd A \code{function} to validate the password enter by the user.
#'  Default is to check for the password to have at least one number, one lowercase,
#'  one uppercase and be of length 6 at least.
#' @param use_token Add a token in the URL to check authentication. Should not be used directly.
#' @param lan An language object. Should not be used directly.
#' 
#' @export
#'
#' @rdname module-password
#'
#' @importFrom htmltools tags
#' @importFrom shiny reactiveValues observeEvent removeUI insertUI icon actionButton
#' @importFrom utils getFromNamespace
pwd_server <- function(input, output, session, user, update_pwd, validate_pwd = NULL, 
                       use_token = FALSE, lan = NULL) {
  
  if(!is.reactive(lan)){
    if(is.null(lan)){
      lan <- reactive(use_language())
    } else {
      lan <- reactive(lan)
    }
  }
  
  if (is.null(validate_pwd)) {
    validate_pwd <- getFromNamespace("validate_pwd", "shinymanager")
  }
  
  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }
  
  password <- reactiveValues(result = FALSE, user = NULL, relog = NULL)
  
  observeEvent(input$update_pwd, {
    password$relog <- NULL
    removeUI(selector = jns("msg_pwd"))
    
    insertUI(
      selector = jns("container-btn-update"),
      ui = tags$div(
        id = ns("spinner_msg_pwd"),
        img(src = "shinymanager/1497.gif", style = "height:30px;"), 
        align = "center"
      ),
      immediate = TRUE 
    )
    
    if (!identical(input$pwd_one, input$pwd_two)) {
      removeUI(selector = jns("spinner_msg_pwd"))
      insertUI(
        selector = jns("result_pwd"),
        ui = tags$div(
          id = ns("msg_pwd"), class = "alert alert-danger",
          icon("triangle-exclamation"), lan()$get("The two passwords are different")
        )
      )
    } else if (!check_new_pwd(user$user, input$pwd_one)) {
      removeUI(selector = jns("spinner_msg_pwd"))
      insertUI(
        selector = jns("result_pwd"),
        ui = tags$div(
          id = ns("msg_pwd"), class = "alert alert-danger",
          icon("triangle-exclamation"), lan()$get("New password cannot be the same as old")
        )
      )
    } else {
      if (!isTRUE(validate_pwd(input$pwd_one))) {
        removeUI(selector = jns("spinner_msg_pwd"))
        insertUI(
          selector = jns("result_pwd"),
          ui = tags$div(
            id = ns("msg_pwd"), class = "alert alert-danger",
            icon("triangle-exclamation"), lan()$get("Password does not respect safety requirements")
          )
        )
      } else {
        
        res_pwd <- update_pwd(user$user, input$pwd_one)
        
        if (isTRUE(res_pwd$result)) {
          password$result <- TRUE
          password$user <- user$user
          removeUI(selector = jns("container-btn-update"))
          insertUI(
            selector = jns("result_pwd"),
            ui = tags$div(
              id = ns("msg_pwd"),
              tags$div(
                class = "alert alert-success",
                icon("check"), lan()$get("Password successfully updated! Please re-login")
              ),
              actionButton(
                inputId = ns("relog"),
                label = lan()$get("Login"),
                width = "100%"
              )
            )
          )
        } else {
          insertUI(
            selector = jns("result_pwd"),
            ui = tags$div(
              id = ns("msg_pwd"), class = "alert alert-danger",
              icon("triangle-exclamation"), lan()$get("Failed to update password")
            )
          )
        }
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$relog, {
    if (isTRUE(use_token)) {
      token <- getToken(session = session)
      .tok$remove(token)
      resetQueryString(session = session)
      session$reload()
    }
    password$relog <- input$relog
  }, ignoreInit = TRUE)
  
  return(password)
}




