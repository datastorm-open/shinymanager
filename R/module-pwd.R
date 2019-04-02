
pwd_ui <- function(id, tag_img = NULL, status = "primary") {

  ns <- shiny::NS(id)

  lan <- use_language()

  htmltools::tagList(
    htmltools::singleton(htmltools::tags$head(
      htmltools::tags$link(href="shinymanager/styles-auth.css", rel="stylesheet"),
      htmltools::tags$script(src = "shinymanager/bindEnter.js")
    )),
    htmltools::tags$div(
      id = ns("pwd-mod"), class = "panel-auth",
      htmltools::tags$br(), htmltools::tags$div(style = "height: 70px;"), htmltools::tags$br(),
      shiny::fluidRow(
        shiny::column(
          width = 4, offset = 4,
          htmltools::tags$div(
            class = paste0("panel panel-", status),
            htmltools::tags$div(
              class = "panel-body",
              htmltools::tags$div(
                style = "text-align: center;",
                if (!is.null(tag_img)) tag_img,
                htmltools::tags$h3(lan$get("Please change your password"))
              ),
              htmltools::tags$br(),
              shiny::passwordInput(
                inputId = ns("pwd_one"),
                label = lan$get("New password:"),
                width = "100%"
              ),
              shiny::passwordInput(
                inputId = ns("pwd_two"),
                label = lan$get("Confirm password:"),
                width = "100%"
              ),
              htmltools::tags$br(),
              htmltools::tags$div(
                id = ns("container-btn-update"),
                shiny::actionButton(
                  inputId = ns("update_pwd"),
                  label = lan$get("Update new password"),
                  width = "100%",
                  class = paste0("btn-", status)
                ),
                htmltools::tags$br(), htmltools::tags$br()
              ),
              htmltools::tags$script(
                sprintf("bindEnter('%s');", ns(""))
              ),
              htmltools::tags$div(id = ns("result_pwd"))
            )
          )
        )
      )
    )
  )
}


pwd_server <- function(input, output, session, update_pwd, use_token = FALSE) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  password <- shiny::reactiveValues(result = FALSE)

  lan <- use_language()

  shiny::observeEvent(input$update_pwd, {
    shiny::removeUI(selector = jns("msg_pwd"))
    if (!identical(input$pwd_one, input$pwd_two)) {
      shiny::insertUI(
        selector = jns("result_pwd"),
        ui = htmltools::tags$div(
          id = ns("msg_pwd"), class = "alert alert-danger",
          shiny::icon("exclamation-triangle"), lan$get("The two passwords are different")
        )
      )
    } else {
      res_pwd <- update_pwd(input$pwd_one)
      if (isTRUE(res_pwd$result)) {
        shiny::removeUI(selector = jns("container-btn-update"))
        shiny::insertUI(
          selector = jns("result_pwd"),
          ui = htmltools::tags$div(
            id = ns("msg_pwd"),
            htmltools::tags$div(
              class = "alert alert-success",
              shiny::icon("check"), lan$get("Password successfully updated! Please re-login")
            ),
            shiny::actionButton(
              inputId = ns("relog"),
              label = lan$get("Login"),
              width = "100%"
            )
          )
        )
      } else {
        shiny::insertUI(
          selector = jns("result_pwd"),
          ui = htmltools::tags$div(
            id = ns("msg_pwd"), class = "alert alert-danger",
            shiny::icon("exclamation-triangle"), lan$get("Failed to update password")
          )
        )
      }
    }
  })

  shiny::observeEvent(input$relog, {
    if (isTRUE(use_token)) {
      query <- shiny::getQueryString()
      token <- query$token
      remove_token(token)
      shiny::updateQueryString(queryString = "?", session = session)
      session$reload()
    }
  })

  return(password)
}




