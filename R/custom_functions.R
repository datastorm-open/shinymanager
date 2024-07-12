#' custom_access_keys
#'
#' Decrypts master_key using users password (entered through shinymanager ui) from the shiny_users.sqlite database
#' Afterwards decrypts requested data with the decrypted master_key from the keys_database.sqlite
#'
#' @param requested_data Name of the secret you want to access
#' @return Decrypted secret for the requested_data
#'
#' @export
custom_access_keys <- function(requested_data){
  # decrypt master_key
  key <- key()
  user_name <- user_name()
  path_to_user_db <- "../../base-data/database/shiny_users.sqlite"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
  master_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", user_name, "'")
  encrypted_master_key <- DBI::dbGetQuery(db, master_key_query)$encrypted_master_key
  master_key <- safer::decrypt_string(encrypted_master_key, key = key)
  DBI::dbDisconnect(db)
  
  # connect to keys_database
  path_to_keys_db <- "../../base-data/database/keys_database.sqlite"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  
  # get the names of all data stored in keys_database
  names_data <- DBI::dbGetQuery(db, "SELECT DISTINCT name FROM keys_database")
  
  # check if the requested data exists in the database
  if (any(grepl(requested_data, names_data$name))) {
    
    # get data
    data <- DBI::dbGetQuery(db, paste("SELECT encrypted_data FROM keys_database WHERE name =", shQuote(requested_data)))
    DBI::dbDisconnect(db)
    
    # decrypt data with master_key and return the secret
    safer::decrypt_string(data$encrypted_data[1], key = master_key)
  } else {
    DBI::dbDisconnect(db)
    return("Error: The requested data does not exist in the database or the name of the requested data is incorrect")
  }
}


#' custom_access_keys_2
#'
#' Loads secret for given name from the keys database and returns the
#' decrypted secret. 
#' When triggered interactively, asks for password of produkt user to decrypt the secret.
#' On the server, the provided password of the signed in user is used.
#'
#' @param name_of_secret Name of the secret to be loaded.
#' @param path_to_keys_db Path to keys_database.sqlite (optional).
#' @param path_to_user_db Path to shiny_users.sqlite (optional).
#' @return Decrypted secret for the requested_data.
#' @export
custom_access_keys_2 <- function(name_of_secret,
                                 path_to_keys_db = "../../base-data/database/keys_database.sqlite",
                                 path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  credentials <- custom_retrieve_credentials()
  user_name <- credentials[[1]]
  password <- credentials[[2]]
  
  get_master_key <- tryCatch({
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
    encrypted_master_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", user_name, "'")
    encrypted_master_key <- DBI::dbGetQuery(db, encrypted_master_key_query)$encrypted_master_key
    DBI::dbDisconnect(db)
    
    master_key <- safer::decrypt_string(encrypted_master_key, key = password)
    list(success = TRUE, master_key = master_key)
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e), "password")
    stop(e)
  })
  
  get_secret <- tryCatch({
    # Connect to keys_database
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
    
    # Get the names of all data stored in keys_database
    names_data <- DBI::dbGetQuery(db, "SELECT DISTINCT name FROM keys_database")
    
    data <- DBI::dbGetQuery(db, paste("SELECT encrypted_data FROM keys_database WHERE name =", shQuote(name_of_secret)))
    DBI::dbDisconnect(db)
      
    # Decrypt data with master_key and return the secret
    secret <- safer::decrypt_string(data$encrypted_data[1], key = get_master_key$master_key)
    
    DBI::dbDisconnect(db)
    #return("Error: The name_of_secret does not exist in the database or name_of_secret is incorrect")
    
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e), "master_key")
    stop(e)
  })
  
  return(secret)
}

# custom_access_keys_2 <- function(name_of_secret,
#                                  path_to_keys_db = "../../base-data/database/keys_database.sqlite",
#                                  path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
#   credentials <- custom_retrieve_credentials()
#   user_name <- credentials[[1]]
#   password <- credentials[[2]]
#   
#   result <- tryCatch({
#     db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
#     
#     encrypted_master_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", user_name, "'")
#     encrypted_master_key <- DBI::dbGetQuery(db, encrypted_master_key_query)$encrypted_master_key
#     
#     DBI::dbDisconnect(db)
#     
#     if(length(encrypted_master_key) == 0){
#       stop("No master key found for this user.")
#     }
#     
#     master_key <- safer::decrypt_string(encrypted_master_key, key = password)
#     
#     if (is.null(master_key) || master_key == "") {
#       stop("Decryption of the master key failed.")
#     }
# 
#     list(success = TRUE, master_key = master_key)
#   }, error = function(e) {
#     message(custom_show_warnings(e$message))
#     if (grepl("Unable to decrypt. Ensure that the input was generated by 'encrypt_string'.", e$message)) {
#       list(success = FALSE, message = "Unable to decrypt. You entered the wrong password.")
#     } else {
#       list(success = FALSE, message = e$message)
#     }
#   })
# 
#   if (!result$success) {
#     return(result$message)
#   }
#   
#   # Connect to keys_database
#   db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
# 
#   # Get the names of all data stored in keys_database
#   names_data <- DBI::dbGetQuery(db, "SELECT DISTINCT name FROM keys_database")
# 
#   # Check if the name_of_secret exists in the database
#   if (any(grepl(name_of_secret, names_data$name))) {
#     # Get data
#     data <- DBI::dbGetQuery(db, paste("SELECT encrypted_data FROM keys_database WHERE name =", shQuote(name_of_secret)))
#     DBI::dbDisconnect(db)
# 
#     # Decrypt data with master_key and return the secret
#     safer::decrypt_string(data$encrypted_data[1], key = result$master_key)
#   } else {
#     DBI::dbDisconnect(db)
#     return("Error: The name_of_secret does not exist in the database or name_of_secret is incorrect")
#   }
# }


#' custom_add_secret
#'
#' Should only be triggered interactively.
#' Encrypts new secret with master_key and stores it in the keys database.
#' Asks for password of produkt user to decrypt master_key and the value of 
#' the new secret to be added.
#'
#' @param name_of_secret Name of the new secret.
#' @param description Description of the new secret (optional).
#' @param path_to_keys_db Path to keys_database.sqlite (optional).
#' @param path_to_user_db Path to shiny_users.sqlite (optional).
#' @return Message indicating the secret has been added.
#' @export
custom_add_secret <- function(name_of_secret, 
                              description = "", 
                              path_to_keys_db = "../../base-data/database/keys_database.sqlite", 
                              path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  # Enter password to decrypt master_key
  key <- getPass::getPass("Enter password for 'produkt': ")
  new_secret <- getPass::getPass("Enter value of new secret:")
  
  # Decrypt master_key
  master_key_query <- "SELECT encrypted_master_key FROM credentials WHERE user = 'produkt'"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
  encrypted_master_key <- DBI::dbGetQuery(db, master_key_query)$encrypted_master_key
  DBI::dbDisconnect(db)
  master_key <- safer::decrypt_string(encrypted_master_key, key = key)

  # Encrypt new secret
  encrypted_data <- safer::encrypt_string(new_secret, key = master_key)

  # Store new secret in keys_database
  put_query <- paste0("INSERT INTO keys_database (name, encrypted_data, description) VALUES ('", name_of_secret, "', '", encrypted_data, "', '", description, "')")
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  DBI::dbExecute(db, put_query)
  DBI::dbDisconnect(db)

  cat("Secret has been added")
}


#' custom_show_secrets
#'
#' Should only be triggered interactively.
#' Retrieves all stored names and descriptions from the keys_database.
#'
#' @param path_to_keys_db Path to keys_database.sqlite (optional).
#' @return Dataframe with names and descriptions of stored secrets.
#'
#' @export
custom_show_secrets <- function(path_to_keys_db = "../../base-data/database/keys_database.sqlite") {
  # Get all stored names and descriptions
  get_query <- "SELECT name, description FROM keys_database"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  data <- DBI::dbGetQuery(db, get_query)
  DBI::dbDisconnect(db)

  return(data)
}


#' custom_delete_secret
#'
#' Should only be triggered interactively.
#' Deletes a secret from the keys_database
#'
#' @param name_of_secret Name of the secret to be deleted.
#' @param path_to_keys_db Path to keys_database.sqlite (optional).
#' @return Message indicating the secret has been deleted.
#' @export
custom_delete_secret <- function(name_of_secret, 
                                 path_to_keys_db = "../../base-data/database/keys_database.sqlite") {
  # Delete secret from keys_database
  put_query <- paste0("DELETE FROM keys_database WHERE name = '", name_of_secret, "'")
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  DBI::dbExecute(db, put_query)
  DBI::dbDisconnect(db)

  cat("Secret has been deleted")
}


#' custom_decrypt_data
#'
#' Decrypts a dataframe with the decrypted content. 
#' The decryption key has to be passed down within a function.
#' Function is deprecated, please use custom_decrypt_data_2.
#'
#' @param decryption_key the secret to decrypt the data with
#' @param encrypted_df encrypted data frames
#'
#' @export
custom_decrypt_data <- function(decryption_key, encrypted_df) {
  
  encrypted_df %>%
    safer::decrypt_object(decryption_key)
}


#' custom_decrypt_data_2
#'
#' Decrypts a dataframe without having to provide the secret.
#'
#' @param encrypted_df Encrypted data frames.
#' @param name_of_secret The name of the secret that decrypts the data.
#' @param path_to_keys_db Path to keys_database.sqlite (optional).
#' @param path_to_user_db Path to shiny_users.sqlite (optional).
#' @return The decrypted dataframe 
#' @export
custom_decrypt_data_2 <- function(encrypted_df,
                                  name_of_secret,
                                  path_to_keys_db = "../../base-data/database/keys_database.sqlite",
                                  path_to_user_db = "../../base-data/database/shiny_users.sqlite") {

  decryption_key <- custom_access_keys_2(name_of_secret,
                                         path_to_keys_db = path_to_keys_db,
                                         path_to_user_db = path_to_user_db)
  
  decrypted_df <- encrypted_df %>%
    safer::decrypt_object(decryption_key)
  
  return(decrypted_df)
}


#' custom_add_user
#'
#' Adds a user on your local device, who has access to sensible data.
#'
#' @param username The username of the new user.
#' @param password The password of the new user.
#' @param include_master_key Boolean indicating if the master key should be accessible to the user (optional).
#' @param path_to_user_db Path to shiny_users.sqlite (optional).
#' @return A message indicating success or the specific error encountered.
#' @export
custom_add_user <- function(username, 
                            password, 
                            include_master_key = TRUE, 
                            path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
   if(!is.character(username) || !is.character(password)) {
    return("Error: Username and password must be characters.")
  } else if(!is.logical(include_master_key)) {
    return("Error: include_master_key must be TRUE or FALSE")
  } else {
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
    if(interactive()) {
      current_user_name <- "produkt"
      key <- getPass::getPass(msg = "Gib das Passwort für den Produktnutzer ein")
    } else {
      current_user_name <- user_name()
      key <- key()
    }
    
    if (include_master_key == TRUE) {
      db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
      master_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", user_name, "'")
      encrypted_master_key <- DBI::dbGetQuery(db, master_key_query)$encrypted_master_key
      master_key <- safer::decrypt_string(encrypted_master_key, key = key)
      DBI::dbDisconnect(db)
      # This is the encrypted_master_key for the new user
      hashed_password <- scrypt::hashPassword(password)
      encrypted_master_key <- safer::encrypt_string(master_key, key = hashed_password)
    } else {
      encrypted_master_key <- ""
    }
    
    insert_query <- paste0("INSERT INTO credentials (user, password, encrypted_master_key) VALUES ('", 
                           username, "', '", password, "', '", encrypted_master_key, "')")
    DBI::dbExecute(db, insert_query)
    DBI::dbDisconnect(db)
    
    return("User has been added successfully.")
  }
}


#' custom_permission
#'
#' Returns an integer telling you hat level of permission the user has.
#' This function does not need any parameters.
#'
#' @param path_to_user_db = "../../base-data/database/shiny_users.sqlite" (optional)
#' @return The Level of Rights the user has
#' @export
custom_permission <- function(path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  
  credentials <- custom_retrieve_credentials()
  user_name <- credentials[[1]]
  
  shiny_users <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
  permission_query <- paste0("SELECT permission FROM credentials WHERE user = '", user_name, "'")
  permission <- DBI::dbGetQuery(shiny_users, permission_query)$permission

  determine_permission_level <- function(permission) {
    level_2 <- c("Admin", "Entwickler", "Geschaeftsfuerung", "Headof", "Verwaltung")
    level_1 <- c("Teamlead")
    
    tryCatch({
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("The package 'dplyr' is not installed. Please install it.")
      }
      
      permission_level <- case_when(
        permission %in% level_2 ~ 2,
        permission %in% level_1 ~ 1,
        TRUE ~ 0
      )
    }, error = function(e) {
      message(custom_show_warnings(conditionMessage(e)))
      permission_level <- NA
    })
    
    return(permission_level)
  }
  
  return(determine_permission_level(permission))
}


#' custom_retrieve_credentials
#'
#' This function is used to retrieve the credentials of the user.
#'
#' @return List which contains the user_name and password
#' @export
custom_retrieve_credentials <- function(){
  retrieve_user_name <- function(){
    tryCatch({
      user <- user_name()
      user
    }, error = function(e) {
      message(custom_show_warnings(conditionMessage(e)))
      user <- "produkt"
      user
    }) 
  }
  
  retrieve_password <- function(){
    tryCatch({
      key <- key()
      key
    }, error = function(e) {
      message(custom_show_warnings(conditionMessage(e)))
      key <- getPass::getPass(msg = "Gib das Passwort für den Produktnutzer ein:")
      key
    })
  } 
  
  user_name <- retrieve_user_name()
  password <- retrieve_password()
  
  return(list(user_name, password))
}


#' custom_show_warnings
#'
#' This function shows the warning of any custom function.
#' If a shiny app is started, the warning pops up on the dashboard.
#' If a function is called locally, the warning is printed on the console. 
#'
#' @param warning The warning a try-catch block throws
#' @param param Parameter used to distinguish between different same errors
#' @return 
#' @export
custom_show_warnings <- function(warning, param){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    library(dplyr)
  }
  
  is_interactive <- interactive()
  
  warning_output <- case_when(
    warning == "could not find function \"user_name\"" || warning == "konnte funktion \"user_name\" nicht finden" && is_interactive
    ~ NA_character_,
    warning == "could not find function \"key\"" || warning == "konnte funktion \"key\" nicht finden" && is_interactive
    ~ NA_character_,
    warning == "Unable to decrypt. Ensure that the input was generated by 'encrypt_string'." && param == "password"
    ~ "Unable to decrypt encrypted_master_key with the given password.",
    warning == "Unable to decrypt. Ensure that the input was generated by 'encrypt_string'." && param == "master_key"
    ~ "Unable to decrypt encrypted_data with the given master_key. Please check the name_of_secret",
    TRUE ~ warning
  )
  
  if (is.na(warning_output)) {
    invisible(NULL)
  } else {
    return(warning_output)
  }
}