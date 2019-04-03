

# Server ------------------------------------------------------------------

function(input, output, session) {

  secure_server(check_credentials = check_credentials(credentials))

  # classic app
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

}
