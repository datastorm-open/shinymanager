

# UI ----------------------------------------------------------------------


secure_app(fluidPage(

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
    plotOutput('plot1'),
    verbatimTextOutput("res_auth")
  )

), enable_admin = TRUE, language = "fr")

