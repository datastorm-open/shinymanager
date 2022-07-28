# Define UI for application that draws a histogram
ui <- navbarPage(title = HTML(paste0('<p style="margin-top: 0.05cm;">', paste0(rep("&nbsp;", 25), collapse = ""), '&nbspshinymanager</p>')), id = "nav-id", collapsible = TRUE,
                 position = "fixed-top", theme = "css/custom.css",
                 header = div(
                   br(), br(), br(), br(),
                   a(href = "https://www.datastorm.fr",
                     target = "_blank", img(src = "img/img-datastorm-logo-white.png", class = "ribbon", style = "margin-left: 0cm;margin-top: 0.1cm;height: 55px")),
                   a(href = "https://github.com/datastorm-open/shinymanager",
                     target = "_blank", img(src = "img/github.png", class = "ribbon", style = "margin-left: 3cm;margin-top: 0cm;height: 60px")),
                   # footer
                   div(class = "ds_app_footer", div(p("copyright © Datastorm 2020", style = "color:white"), align = "center")),
                 ),
                 windowTitle = "shinymanager",
                 
                 tabPanel("Démo",
                          fluidRow(
                            div(
                              h2("You're welcome !"),
                              img(src = "figures/shinymanager.png", style = "height: 250px"),
                              align = "center")
                          ),
                          h3("User information :"),
                          verbatimTextOutput("auth_output"),
                          h3("input$shinymanager_language :"),
                          verbatimTextOutput("shinymanager_language"),
                          h3("input$shinymanager_where :"),
                          verbatimTextOutput("shinymanager_where")
                 ),
                 
                 tabPanel("Code",
                          fluidRow(
                            tabsetPanel(
                              tabPanel("Init Database",
                                       includeMarkdown("www/script/init_db.md")
                              ),
                              tabPanel("server.R",
                                       includeMarkdown("www/script/server.md")
                              ),
                              tabPanel("ui.R",
                                       includeMarkdown("www/script/ui.md")
                              )
                            )
                          )
                          
                 ),
                 
                 br(), br(), br()
)

secure_app(ui,
           enable_admin = TRUE, 
           language = "fr", 
           choose_language = TRUE, 
           status = "warning",
           tags_top = 
             tags$div(
               tags$img(
                 src = "img/logoDS.png", width = 100
                 # le chemin pourrait être local (www/img/image.jpg -> "img/image.jpg")
               )
             ),
           # information en bas ?
           tags_bottom = tags$div(
             tags$p(
               a(href = "https://github.com/datastorm-open/shinymanager",
                 target = "_blank", img(src = "img/github.png", class = "ribbon", style = "height: 60px")
               ),
               tags$p("user : shiny / pwd : shiny or shinymanager / shinymanager (admin)"),
             ), align = "center"
           ),
           # ui background ?
           # https://developer.mozilla.org/fr/docs/Web/CSS/background
           background  = "center/20% url('figures/shinymanager.png');"
)
