#' custom_access_keys
#'
#' Decrypts master_key using users password (entered through Shinymanager) from the shiny_users.sqlite database.
#' Afterwards decrypts requested data with the decrypted master_key from the keys_database.sqlite.
#'
#' @param requested_data Name of the secret you want to access.
#' @return Decrypted secret for the requested_data.
#' @export
custom_access_keys <- function(requested_data){
  # Decrypt master_key
  key <- key()
  user_name <- user_name()
  path_to_user_db <- "../../base-data/database/shiny_users.sqlite"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
  master_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", user_name, "'")
  encrypted_master_key <- DBI::dbGetQuery(db, master_key_query)$encrypted_master_key
  master_key <- safer::decrypt_string(encrypted_master_key, key = key)
  DBI::dbDisconnect(db)

  # Connect to keys_database
  path_to_keys_db <- "../../base-data/database/keys_database.sqlite"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)

  # Get the names of all data stored in keys_database
  names_data <- DBI::dbGetQuery(db, "SELECT DISTINCT name FROM keys_database")

  # Check if the requested data exists in the database
  if (any(grepl(requested_data, names_data$name))) {
    # Get data
    data <- DBI::dbGetQuery(db, paste("SELECT encrypted_data FROM keys_database WHERE name =", shQuote(requested_data)))
    DBI::dbDisconnect(db)

    # Decrypt data with master_key and return the secret
    safer::decrypt_string(data$encrypted_data[1], key = master_key)
  } else {
    DBI::dbDisconnect(db)
    return("Error: The requested data does not exist in the database or the name of the requested data is incorrect")
  }
}



#' custom_add_secret
#'
#' Encrypts new secret with master_key and stores it in the keys_database.
#'
#' @param name Name of the new secret.
#' @param new_secret Secret to be stored.
#' @param description Description of the new secret.
#' @param path_to_keys_db Path to keys_database.sqlite.
#' @param path_to_user_db Path to shiny_users.sqlite.
#' @return Message indicating the secret has been added.
#' @export
custom_add_secret <- function(name, 
                              new_secret, 
                              description = "", 
                              path_to_keys_db = "../../base-data/database/keys_database.sqlite", 
                              path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  # Enter password to decrypt master_key
  key <- getPass::getPass("Enter password for 'produkt': ")

  # Decrypt master_key
  master_key_query <- "SELECT encrypted_master_key FROM credentials WHERE user = 'produkt'"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
  encrypted_master_key <- DBI::dbGetQuery(db, master_key_query)$encrypted_master_key
  DBI::dbDisconnect(db)
  master_key <- safer::decrypt_string(encrypted_master_key, key = key)

  # Encrypt new secret
  encrypted_data <- safer::encrypt_string(new_secret, key = master_key)

  # Store new secret in keys_database
  put_query <- paste0("INSERT INTO keys_database (name, encrypted_data, description) VALUES ('", name, "', '", encrypted_data, "', '", description, "')")
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  DBI::dbExecute(db, put_query)
  DBI::dbDisconnect(db)

  cat("Secret has been added")
}



#' custom_show_secrets
#'
#' Retrieves all stored names and descriptions from the keys_database.
#'
#' @param path_to_keys_db Path to keys_database.sqlite.
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
#' Deletes a secret from the keys_database
#'
#' @param secret Name of the secret to be deleted
#' @param path_to_keys_db Path to keys_database.sqlite
#' @return Message indicating the secret has been deleted.
#' @export
custom_delete_secret <- function(secret, path_to_keys_db = "../../base-data/database/keys_database.sqlite") {
  # Delete secret from keys_database
  put_query <- paste0("DELETE FROM keys_database WHERE name = '", secret, "'")
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  DBI::dbExecute(db, put_query)
  DBI::dbDisconnect(db)

  cat("Secret has been deleted")

}


#' custom_load_secret
#'
#' Loads a secret from the database on you local device 
#'
#' @param name_of_secret Name of the secret to be loaded.
#' @param path_to_keys_db Path to keys_database.sqlite
#' @param path_to_user_db Path to shiny_users.sqlite.
#' @return The decrypted secret.
#' @export
custom_load_secret <- function(name_of_secret, 
                               path_to_keys_db = "../../base-data/database/keys_database.sqlite",
                               path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  username <- "produkt" # In Zukunft ändern
  passwd <- getPass::getPass(msg = "Gib das Passwort für den Produktnutzer ein")
  
  # Query encrypted_secret from keys_database
  keys_db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  encrypted_secret_query <- paste0("SELECT encrypted_data FROM keys_database WHERE name = '", name_of_secret, "'")
  encrypted_secret <- DBI::dbGetQuery(keys_db, encrypted_secret_query)$encrypted_data
  DBI::dbDisconnect(keys_db)
  
  # Query decrypt_key_encrypted from keys_database
  shiny_users <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
  decrypt_key_query <- paste0("SELECT encrypted_master_key FROM credentials WHERE user = '", username, "'")
  decrypt_key_encrypted <- DBI::dbGetQuery(shiny_users, decrypt_key_query)$encrypted_master_key
  DBI::dbDisconnect(shiny_users)
  
  # Decrypt master key
  decrypt_key <- safer::decrypt_string(decrypt_key_encrypted, key = passwd)
  
  # Decrypt secret
  secret <- safer::decrypt_string(encrypted_secret, key = decrypt_key)
  
  return(secret)
}

Billomatics::authentication_process()
#' custom_decrypt_data
#'
#' Decrypts a dataframe with the provided decryption key.
#'
#' @param decryption_key The secret to decrypt the data with.
#' @param encrypted_df Encrypted data frames.
#' @return A dataframe with the decrypted content.
#' @export
custom_decrypt_data <- function(decryption_key, encrypted_df) {

  encrypted_df %>%
    safer::decrypt_object(decryption_key)
}
