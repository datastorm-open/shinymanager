server <- function(input, output, session) {

    res_auth <- secure_server(
        check_credentials = check_credentials(
            "database.sqlite",
            passphrase = "passphrase_wihtout_keyring"
        ),
        send_mail = function(user, email, temp_password) {
            message(sprintf("[MAIL] to=%s | user=%s | temp_pwd=%s", email, user, temp_password))
        })
  
    # recuperation des infos utilisateurs
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    
    # si besoin, des inputs sont créés
    output$shinymanager_language_ <- renderPrint({
        input$shinymanager_language
    })
    
    observe({
      print(input$shinymanager_language)
      print(input$shinymanager_where)
      
    })
    output$shinymanager_where_ <- renderPrint({
        input$shinymanager_where
    })
    
}