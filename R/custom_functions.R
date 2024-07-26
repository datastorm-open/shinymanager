#' custom_access_keys
#'
#' Decrypts master_key using users password (entered through shinymanager ui) from the shiny_users.sqlite database
#' Afterwards decrypts requested data with the decrypted master_key from the keys_database.sqlite
#' Function is deprecated, please use custom_access_keys_2.
#'
#' @param requested_data Name of the secret you want to access
#' @return Decrypted secret for the requested_data
#'
#' @export
custom_access_keys <- function(requested_data){
  is_interactive <- custom_interactive()
  if(is_interactive) {
   warning("The function custom_access_keys is deprecated. Please use custom_access_keys_2 instead.") 
  } else if (!interactive()) {
    # Running on Server, no warning should be visible
  } else {
    shiny::showModal(modalDialog(
      title = "Attention",
      "The function custom_access_keys is deprecated. Please use custom_access_keys_2 instead.",
      easyClose = TRUE,
      footer = NULL
    ))
  }
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
    on.exit({
      if (DBI::dbIsValid(db)) {
        DBI::dbDisconnect(db)
      }
    }, add = TRUE)
    
    encrypted_master_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", user_name, "'")
    encrypted_master_key <- DBI::dbGetQuery(db, encrypted_master_key_query)$encrypted_master_key
    
    master_key <- safer::decrypt_string(encrypted_master_key, key = password)
    list(success = TRUE, master_key = master_key)
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e), "password", user_name)
    stop(e)
  })
  
  get_secret <- tryCatch({
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
    on.exit({
      if (DBI::dbIsValid(db)) {
        DBI::dbDisconnect(db)
      }
    }, add = TRUE)
    
    names_data <- DBI::dbGetQuery(db, "SELECT DISTINCT name FROM keys_database")
    
    data <- DBI::dbGetQuery(db, paste("SELECT encrypted_data FROM keys_database WHERE name =", shQuote(name_of_secret)))
    DBI::dbDisconnect(db)
    
    if (!any(grepl(name_of_secret, names_data$name))){
      stop("Error: The name_of_secret does not exist in the database or the name_of_secret is incorrect")
    }
    secret <- safer::decrypt_string(data$encrypted_data[1], key = get_master_key$master_key)
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e), "master_key")
    stop(e)
  })
  
  return(secret)
}


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
  master_key <- tryCatch({
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
    master_key_query <- "SELECT encrypted_master_key FROM credentials WHERE user = 'produkt'"
    encrypted_master_key <- DBI::dbGetQuery(db, master_key_query)$encrypted_master_key
    safer::decrypt_string(encrypted_master_key, key = key)
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e), "password")
    stop(e)
  })

  # Encrypt new secret
  encrypted_data <- safer::encrypt_string(new_secret, key = master_key)
  tryCatch({
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
    put_query <- paste0("INSERT INTO keys_database (name, encrypted_data, description) VALUES ('", name_of_secret, "', '", encrypted_data, "', '", description, "')")
    DBI::dbExecute(db, put_query)
    cat("Secret has been added")
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e), "master_key")
    stop(e)
  })
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
  tryCatch({
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
    get_query <- "SELECT name, description FROM keys_database"
    data <- DBI::dbGetQuery(db, get_query)
    return(data)
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e))
    stop(e)
  })
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
  tryCatch({
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
    delete_query <- paste0("DELETE FROM keys_database WHERE name = '", name_of_secret, "'")
    DBI::dbExecute(db, put_query)
    cat("Secret has been deleted")
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e))
    stop(e)
  })
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
  is_interactive <- custom_interactive()
  if(is_interactive) {
    warning("The function custom_decrypt_data is deprecated. Please use custom_decrypt_data_2 instead.")
  } else if (!interactive()) {
    # Running on Server, no warning should be visible
  } else {
    shiny::showModal(modalDialog(
      title = "Attention",
      "The function custom_decrypt_data is deprecated. Please use custom_decrypt_data_2 instead.",
      easyClose = TRUE,
      footer = NULL
    ))
  }
  
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
#' @return The decrypted dataframe.
#' @export
custom_decrypt_data_2 <- function(encrypted_df,
                                  name_of_secret,
                                  path_to_keys_db = "../../base-data/database/keys_database.sqlite",
                                  path_to_user_db = "../../base-data/database/shiny_users.sqlite") {

  tryCatch({
    decryption_key <- custom_access_keys_2(name_of_secret,
                                           path_to_keys_db = path_to_keys_db,
                                           path_to_user_db = path_to_user_db)
    decrypted_df <- safer::decrypt_object(encrypted_df, decryption_key)
    return(decrypted_df)  
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e))
    stop(e)
  })  
}


#' custom_encrypt_data
#'
#' Encrypts a dataframe without having to provide the secret.
#'
#' @param data_df The data frame to be encrypted.
#' @param name_of_secret The name of the secret that decrypts the data.
#' @param path_to_keys_db Path to keys_database.sqlite (optional).
#' @param path_to_user_db Path to shiny_users.sqlite (optional).
#' @return The encrypted dataframe .
#' @export
custom_encrypt_data <- function(data_df,
                                name_of_secret,
                                path_to_keys_db = "../../base-data/database/keys_database.sqlite",
                                path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  tryCatch({
    decryption_key <- custom_access_keys_2(name_of_secret,
                                           path_to_keys_db = path_to_keys_db,
                                           path_to_user_db = path_to_user_db)
    encrypted_df <- safer::encrypt_object(data_df, decryption_key)
    return(encrypted_df)  
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e))
    stop(e)
  })  
}



#' custom_add_user
#'
#' Adds a user on your local device, who has access to sensible data.
#'
#' @param include_master_key Boolean indicating if the master key should be accessible to the user (optional).
#' @param path_to_user_db Path to shiny_users.sqlite (optional).
#' @return A message indicating success or the specific error encountered.
#' @export
custom_add_user <- function(include_master_key = TRUE, 
                            path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  username <- getPass::getPass("Enter the username for the new user: ")
  password <- getPass::getPass("Enter the password for the new user:")
  
  tryCatch({
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
    
    credentials <- custom_retrieve_credentials()
    user_name <- credentials[[1]]
    key <- credentials[[2]]
    
    encrypted_master_key <- ""
    if (include_master_key) {
      master_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", user_name, "'")
      result <- DBI::dbGetQuery(db, master_key_query)
      
      if (nrow(result) == 0) {
        stop(sprintf("The user '%s' was not found in the users database.", user_name))
      }
      
      encrypted_master_key <- result$encrypted_master_key[1]
      master_key <- safer::decrypt_string(encrypted_master_key, key = key)
      hashed_password <- scrypt::hashPassword(password)
      encrypted_master_key <- safer::encrypt_string(master_key, key = hashed_password)
    }
    
    insert_query <- "INSERT INTO credentials (user, password, encrypted_master_key) VALUES (?, ?, ?)"
    DBI::dbExecute(db, insert_query, params = list(username, scrypt::hashPassword(password), encrypted_master_key))
    
    return("User has been added successfully.")
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e), "password")
    stop(e)
  })
}


#' custom_permission_level
#'
#' Returns an integer telling you hat level of permission the user has.
#' This function does not need any parameters.
#'
#' @param path_to_user_db The path to the user database. Default is "../../base-data/database/shiny_users.sqlite". (optional)
#' @return An integer representing the user's permission level.
#' @export
custom_permission_level <- function(path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  
  user_name <- custom_retrieve_credentials(password = FALSE)[[1]]
  
  permission <- tryCatch({
    db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
    
    permission_query <- paste0("SELECT permission FROM credentials WHERE user = '", user_name, "'")
    result <- DBI::dbGetQuery(db, permission_query)
    
    if (length(result$permission) == 0) {
      stop(sprintf("The user '%s' was not found in the users database.", user_name))
    }
    
    result$permission
  }, error = function(e) {
    e$message <- custom_show_warnings(conditionMessage(e), "user_db")
    stop(e)
  })
  

  determine_permission_level <- function(permission) {
    permission_level <- tryCatch({
      dplyr::case_when(
        permission %in% c("Admin", "Entwickler", "Geschaeftsfuerung", "Headof", "Verwaltung") ~ 2,
        permission %in% c("Teamlead") ~ 1,
        TRUE ~ 0
      )
    }, error = function(e) {
      e$message <- custom_show_warnings(conditionMessage(e))
      stop(e)
    })
    
    return(permission_level)
  }
  
  return(determine_permission_level(permission))
}


#' custom_username
#'
#' This function is used to retrieve the username of the user.
#'
#' @return The username
#' @export
custom_username <- function() {
  user_name <- custom_retrieve_credentials(password = FALSE)[[1]]
  return(user_name)
}


#' custom_retrieve_credentials
#'
#' This function is used to retrieve the credentials of the user.
#'
#' @param username Boolean indicating if the user_name should be retrieved. Default is TRUE (optional).
#' @param password Boolean indicating if the password should be retrieved. Default is TRUE (optional).
#' @return List which contains the user_name and password
custom_retrieve_credentials <- function(username = TRUE, password = TRUE){
  is_interactive <- custom_interactive()
  
  retrieve_user_name <- function(){
    if (!username) {
      return()
    }
    if (is_interactive) {
      return("produkt")
    } else {
      return(user_name())
    }
  }
  
  retrieve_password <- function(){
    if(!password) {
      return()
    }
    if(is_interactive) {
      return(getPass::getPass(msg = "Gib das Passwort für den Produktnutzer ein:"))
    } else {
      return(key()) 
    }
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
#' @param param Parameter used to distinguish between errors (Optional).
#' @param username Used to print the username if not found in the database (Optional).
#' @return The modified warning message or NULL if the warning is to be suppressed.
custom_show_warnings <- function(warning, param = NA, username = NA){
  warning_output <- dplyr::case_when(
    warning == "Unable to decrypt. Ensure that the input was generated by 'encrypt_string'." && param == "password" ~ "Unable to decrypt encrypted_master_key with the given password.",
    warning == "Unable to decrypt. Ensure that the input was generated by 'encrypt_string'." && param == "master_key" ~ "Unable to decrypt encrypted_data with the given master_key.",
    warning == "string is not a string (a length one character vector). or string is not a raw vector" && param == "password" || warning == "Zeichenkette ist keine Zeichenkette (ein Vektor der Länge eins). oder Zeichenkette ist kein Raw-Vektor." && param == "password" ~ sprintf("The user '%s' was not found in the users database. This user is important to manage the user and key database.", username),
    TRUE ~ warning
  )
  return(warning_output)
}


#' custom_interactive
#'
#' This function checks if a function is called from a shiny app or from the console.
#' It works just like interactive(), but returns FALSE if running from a shiny app.
#'
#' @return Boolean value indicating if a function is called from a shiny app or console
custom_interactive <- function(){
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    return(FALSE)
  }
  return(interactive())
}

#' custom_load_shiny_module_data
#'
#' @param data_file This can be an Data Frame for example TestData or the path (string) to the data file
#' @param name_of_secret Only necessary if the data is encrypted: The name of the secret in the keys database that decrypts the data
#' @return Returns the loaded data as a dataframe.
#' @details If the data is encrypted (detected by it being of type `raw`), the function attempts to decrypt it using `shinymanager::custom_decrypt_data_2()`.
#' @examples
#' # Load unencrypted data frame
#' data <- custom_load_shiny_module_data(TestData)
#' 
#' # Load encrypted data and decrypt it
#' data <- custom_load_shiny_module_data("cars_encrypted.RDS", name_of_secret = "billomat_db_key")
custom_load_data_in_module <- function(data_file, name_of_secret) {
  # ---- start ---- #
  # Use data_file as dataframe or path
  if (is.data.frame(data_file)) {
    data_df <- data_file
  } else if (is.character(data_file)) {
    data_df <- readRDS(data_file)
  } else {
    stop("The 'data_file'-Parameter has to be an Path (String) or an data frame.")
  }
  
  # check if data is encrypted (raw type)
  if (is.raw(data_df)) {
    # Ensure name_of_secret is provided for decryption
    if (!is.character(name_of_secret)) { # Test if secret available
      stop(
        "The data in data_df is encrypted. In order to decrypt the data, you have to pass the correct name_of_secret"
      )
    } else {
      data_df <- shinymanager::custom_decrypt_data_2(data_df, name_of_secret)
    }
  }
  
  return(data_df)
  
}