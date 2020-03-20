

# Server ------------------------------------------------------------------

function(input, output, session) {

  # Call secure_server() with DB info
  # its' an example, don't put password in clear like that
  auth_out <- secure_server(
    check_credentials = check_credentials(
      db = "credentials.sqlite",
      passphrase = "supersecret"
    ), timeout = 0, 
    inputs_list = list(group = list(fun = "selectInput", 
                                    args = list(choices = c("all", "restricted"), 
                                                multiple = TRUE, 
                                                selected = c("all", "restricted")
                                                )
                                    )
                       )
  )

  observe({
    print(input$shinymanager_where)
    print(input$shinymanager_language)
  })

  output$res_auth <- renderPrint({
    reactiveValuesToList(auth_out)
  })

  # classic app
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

}
