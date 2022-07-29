

# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Basic tabs"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("gauge")),
      menuItem("Widgets", tabName = "widgets", icon = icon("table-cells"))
    )
  ),

  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),

                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),

      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)


secure_app(ui)

