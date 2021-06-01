

# UI ----------------------------------------------------------------------


ui <- function(request) {
  fluidPage(
    
    # classic app
    headerPanel('Iris k-means clustering'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris)),
      selectInput('ycol', 'Y Variable', names(iris),
                  selected=names(iris)[[2]]),
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9),
      bookmarkButton(id = "bookmark1")
    ),
    mainPanel(
      plotOutput('plot1'),
      verbatimTextOutput("res_auth")
    )
    
  )
}

secure_app(ui, enable_admin = TRUE, choose_language = T )

# ?_inputs_&xcol=%22Petal.Width%22
# enableBookmarking(store = "url")
