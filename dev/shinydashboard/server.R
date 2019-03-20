

# Server ------------------------------------------------------------------

function(input, output, session) {

  manage_auth_server(session, check_credentials = check_credentials_p)

  # classic app
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

}
