#' custom_access_keys
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
custom_access_keys <- function(name_of_secret,
                               path_to_keys_db = "../../base-data/database/keys_database.sqlite",
                               path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  if(interactive()) {
    user_name <- "produkt"
    key <- getPass::getPass(msg = "Gib das Passwort für den Produktnutzer ein")
  } else {
    # Decrypt master_key
    key <- key()
    user_name <- user_name()
  }
  
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
  master_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", user_name, "'")
  encrypted_master_key <- DBI::dbGetQuery(db, master_key_query)$encrypted_master_key
  master_key <- safer::decrypt_string(encrypted_master_key, key = key)
  DBI::dbDisconnect(db)

  # Connect to keys_database
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)

  # Get the names of all data stored in keys_database
  names_data <- DBI::dbGetQuery(db, "SELECT DISTINCT name FROM keys_database")

  # Check if the name_of_secret exists in the database
  if (any(grepl(name_of_secret, names_data$name))) {
    # Get data
    data <- DBI::dbGetQuery(db, paste("SELECT encrypted_data FROM keys_database WHERE name =", shQuote(name_of_secret)))
    DBI::dbDisconnect(db)

    # Decrypt data with master_key and return the secret
    safer::decrypt_string(data$encrypted_data[1], key = master_key)
  } else {
    DBI::dbDisconnect(db)
    return("Error: The name_of_secret does not exist in the database or name_of_secret is incorrect")
  }
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
#' Decrypts a dataframe without having to provide the secret.
#'
#' @param encrypted_df Encrypted data frames.
#' @param name_of_secret The name of the secret that decrypts the data (optional).
#' @param path_to_keys_db Path to keys_database.sqlite (optional).
#' @param path_to_user_db Path to shiny_users.sqlite (optional).
#' @return The decrypted dataframe 
#' @export
custom_decrypt_data <- function(encrypted_df,
                                name_of_secret = "billomat_db_key", 
                                path_to_keys_db = "../../base-data/database/keys_database.sqlite",
                                path_to_user_db = "../../base-data/database/shiny_users.sqlite") {

  decryption_key <- custom_access_keys(name_of_secret,
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


#' #' custom_permission
#' #'
#' #' 
#' #' @param username = "../../base-data/database/shiny_users.sqlite"
#' #' @param path_to_user_db = "../../base-data/database/shiny_users.sqlite" (optional)
#' #' @return The Level of Rights the User has
#' #' @export
#' custom_permission <- function(username,
#'                               path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
#'   shiny_users <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
#'   permissions_query <- paste0("SELECT permission FROM credentials WHERE user = '", username, "'")
#'   decrypt_key_encrypted <- DBI::dbGetQuery(shiny_users, decrypt_key_query)$encrypted_master_key
#'   
#'   
#'   
#'   
#'   tables <- DBI::dbListTables(shiny_users)
#'   shiny_users <- shinymanager::read_db_decrypt(shiny_users, "credentials")
#' }