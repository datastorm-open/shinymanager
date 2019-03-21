
pwd_ui <- function(id, labels = pwd_labels(), tag_img = NULL, status = "primary") {

  ns <- shiny::NS(id)

  .globals$labels_pwd <- labels
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
                htmltools::tags$h3(labels$change_password)
              ),
              htmltools::tags$br(),
              shiny::passwordInput(
                inputId = ns("pwd_one"),
                label = labels$new_password,
                width = "100%"
              ),
              shiny::passwordInput(
                inputId = ns("pwd_two"),
                label = labels$confirm_password,
                width = "100%"
              ),
              htmltools::tags$br(),
              htmltools::tags$div(
                id = ns("container-btn-update"),
                shiny::actionButton(
                  inputId = ns("update_pwd"),
                  label = labels$update_pwd,
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

  shiny::observeEvent(input$update_pwd, {
    shiny::removeUI(selector = jns("msg_pwd"))
    if (!identical(input$pwd_one, input$pwd_two)) {
      shiny::insertUI(
        selector = jns("result_pwd"),
        ui = htmltools::tags$div(
          id = ns("msg_pwd"), class = "alert alert-danger",
          shiny::icon("exclamation-triangle"), .globals$labels_pwd$error_not_identical
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
              shiny::icon("check"), .globals$labels_pwd$success_update
            ),
            shiny::actionButton(
              inputId = ns("relog"),
              label = .globals$labels_pwd$login,
              width = "100%"
            )
          )
        )
      } else {
        shiny::insertUI(
          selector = jns("result_pwd"),
          ui = htmltools::tags$div(
            id = ns("msg_pwd"), class = "alert alert-danger",
            shiny::icon("exclamation-triangle"), .globals$labels_pwd$error_update
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


pwd_labels <- function(change_password = "Please change your password",
                       new_password = "New password:",
                       confirm_password = "Confirm password:",
                       update_pwd = "Update new password",
                       success_update = "Password successfully updated! Please re-login",
                       error_not_identical = "The two passwords are different",
                       error_update = "Failed to update password",
                       login = "Login") {
  list(
    change_password = change_password,
    new_password = new_password,
    confirm_password = confirm_password,
    update_pwd = update_pwd,
    success_update = success_update,
    error_not_identical = error_not_identical,
    error_update = error_update,
    login = login
  )
}



