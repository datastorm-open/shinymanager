
#' @importFrom htmltools tagList singleton tags
#' @importFrom shiny NS fluidRow column passwordInput actionButton
pwd_ui <- function(id, tag_img = NULL, status = "primary") {

  ns <- NS(id)

  lan <- use_language()

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
              tags$div(id = ns("result_pwd"))
            )
          )
        )
      )
    )
  )
}


#' @importFrom htmltools tags
#' @importFrom shiny reactiveValues observeEvent removeUI insertUI icon actionButton
pwd_server <- function(input, output, session, user, update_pwd, use_token = FALSE) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  password <- reactiveValues(result = FALSE)

  lan <- use_language()

  observeEvent(input$update_pwd, {
    removeUI(selector = jns("msg_pwd"))
    if (!identical(input$pwd_one, input$pwd_two)) {
      insertUI(
        selector = jns("result_pwd"),
        ui = tags$div(
          id = ns("msg_pwd"), class = "alert alert-danger",
          icon("exclamation-triangle"), lan$get("The two passwords are different")
        )
      )
    } else {
      res_pwd <- update_pwd(user$user, input$pwd_one)
      if (isTRUE(res_pwd$result)) {
        removeUI(selector = jns("container-btn-update"))
        insertUI(
          selector = jns("result_pwd"),
          ui = tags$div(
            id = ns("msg_pwd"),
            tags$div(
              class = "alert alert-success",
              icon("check"), lan$get("Password successfully updated! Please re-login")
            ),
            actionButton(
              inputId = ns("relog"),
              label = lan$get("Login"),
              width = "100%"
            )
          )
        )
      } else {
        insertUI(
          selector = jns("result_pwd"),
          ui = tags$div(
            id = ns("msg_pwd"), class = "alert alert-danger",
            icon("exclamation-triangle"), lan$get("Failed to update password")
          )
        )
      }
    }
  })

  observeEvent(input$relog, {
    if (isTRUE(use_token)) {
      token <- getToken(session = session)
      remove_token(token)
      resetQueryString(session = session)
      session$reload()
    }
    password$relog <- input$relog
  })

  return(password)
}




