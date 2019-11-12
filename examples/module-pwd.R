if (interactive()) {

  library(shiny)
  library(shinymanager)

  ui <- fluidPage(
    tags$h2("Change password module"),
    actionButton(
      inputId = "ask", label = "Ask to change password"
    ),
    verbatimTextOutput(outputId = "res_pwd")
  )

  server <- function(input, output, session) {

    observeEvent(input$ask, {
      insertUI(
        selector = "body",
        ui = tags$div(
          id = "module-pwd",
          pwd_ui(id = "pwd")
        )
      )
    })

    output$res_pwd <- renderPrint({
      reactiveValuesToList(pwd_out)
    })

    pwd_out <- callModule(
      module = pwd_server,
      id = "pwd",
      user = reactiveValues(user = "me"),
      update_pwd = function(user, pwd) {
        # store the password somewhere
        list(result = TRUE)
      }
    )

    observeEvent(pwd_out$relog, {
      removeUI(selector = "#module-pwd")
    })
  }

  shinyApp(ui, server)

}
