library(shiny)
library(shinymanager)

ui <- fluidPage(

  tags$h1("FAB button"),

  tags$p("FAB button:"),
  verbatimTextOutput(outputId = "res_fab"),

  tags$p("Logout button:"),
  verbatimTextOutput(outputId = "res_logout"),

  tags$p("Info button:"),
  verbatimTextOutput(outputId = "res_info"),

  fab_button(
    actionButton(
      inputId = "logout",
      label = "Logout",
      icon = icon("arrow-right-from-bracket")
    ),
    actionButton(
      inputId = "info",
      label = "Information",
      icon = icon("info")
    ),
    inputId = "fab"
  )

)

server <- function(input, output, session) {

  output$res_fab <- renderPrint({
    input$fab
  })

  output$res_logout <- renderPrint({
    input$logout
  })

  output$res_info <- renderPrint({
    input$info
  })

}

if (interactive()) {
  shinyApp(ui, server)
}
