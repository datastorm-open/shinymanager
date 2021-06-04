if (interactive()) {
  
  library(shiny)
  library(shinymanager)
  
  # data.frame with credentials info
  # credentials <- data.frame(
  #   user = c("fanny", "victor"),
  #   password = c("azerty", "12345"),
  #   comment = c("alsace", "auvergne"),
  #   stringsAsFactors = FALSE
  # )
  
  # you can hash the password using scrypt
  # and adding a column is_hashed_password
  # data.frame with credentials info
  credentials <- data.frame(
    user = c("fanny", "victor"),
    password = c(scrypt::hashPassword("azerty"), scrypt::hashPassword("12345")),
    is_hashed_password = TRUE,
    comment = c("alsace", "auvergne"),
    stringsAsFactors = FALSE
  )
  
  # app
  ui <- fluidPage(
    
    # authentication module
    auth_ui(
      id = "auth",
      # add image on top ?
      tags_top = 
        tags$div(
          tags$h4("Demo", style = "align:center"),
          tags$img(
            src = "https://www.r-project.org/logo/Rlogo.png", width = 100
        )
      ),
      # add information on bottom ?
      tags_bottom = tags$div(
        tags$p(
          "For any question, please  contact ",
          tags$a(
            href = "mailto:someone@example.com?Subject=Shiny%20aManager",
            target="_top", "administrator"
          )
        )
      ),
      # change auth ui background ?
      # https://developer.mozilla.org/fr/docs/Web/CSS/background
      background  = "linear-gradient(rgba(0, 0, 255, 0.5),
                       rgba(255, 255, 0, 0.5)),
                       url('https://www.r-project.org/logo/Rlogo.png');", 
      # set language ?
      lan = use_language("fr")
    ),
    
    # result of authentication
    verbatimTextOutput(outputId = "res_auth"),
    
    # classic app
    headerPanel('Iris k-means clustering'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris)),
      selectInput('ycol', 'Y Variable', names(iris),
                  selected=names(iris)[[2]]),
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9)
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
  
  server <- function(input, output, session) {
    
    # authentication module
    auth <- callModule(
      module = auth_server,
      id = "auth",
      check_credentials = check_credentials(credentials)
    )
    
    output$res_auth <- renderPrint({
      reactiveValuesToList(auth)
    })
    
    # classic app
    selectedData <- reactive({
      
      req(auth$result)  # <---- dependency on authentication result
      
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
  
  shinyApp(ui, server)
  
}
