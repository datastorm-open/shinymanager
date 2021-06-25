server <- function(input, output, session) {

    res_auth <- secure_server(
        check_credentials = check_credentials(
            "database.sqlite",
            passphrase = "passphrase_wihtout_keyring"
        )
    )
    
    # recuperation des infos utilisateurs
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    
    # si besoin, des inputs sont créés
    output$shinymanager_language <- renderPrint({
        input$shinymanager_language
    })
    
    output$shinymanager_where <- renderPrint({
        input$shinymanager_where
    })
    
}
