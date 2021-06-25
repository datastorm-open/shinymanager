``` r
ui <- navbarPage(
  # your app
)

secure_app(ui,
           enable_admin = TRUE, 
           language = "fr", 
           choose_language = TRUE, 
           status = "warning",
           # top
           tags_top = 
             tags$div(
               tags$img(
                 src = "img/logoDS.png", width = 100
               )
             ),
           # bottom
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
```
