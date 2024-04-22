#' custom_access_keys
#'
#' decryps master key using users password and then uses master key to decrypt the requested data
#' 
#' @param requested_data name of the key you want to access
#' @param encrypted_master_key master_key decrypted with users password
#' @param key users password
#' @return decryption key for a data set
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



#' custom_add_secret
#'
#' encrypts and stores new secret in keys_database
#' 
#' @param name name of the new secret
#' @param new_secret new secret
#' @param description description
#' @param path_to_keys_db path to keys_database.sqlite
#' @param path_to_user_db path to shiny_users.sqlite
#'
#' @export
custom_add_secret <- function(name, new_secret, description = "", path_to_keys_db = "../../base-data/database/keys_database.sqlite", path_to_user_db = "../../base-data/database/shiny_users.sqlite") {
  
  cat("enter password for produkt: ")
  key <- readline()
  
  # encrypt new secret
  master_key_query <- "SELECT encrypted_master_key FROM credentials WHERE user = 'produkt'"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_user_db)
  encrypted_master_key <- DBI::dbGetQuery(db, master_key_query)$encrypted_master_key
  DBI::dbDisconnect(db)
  master_key <- safer::decrypt_string(encrypted_master_key, key = key)
  encrypted_data <- safer::encrypt_string(new_secret, key = master_key)
  
  # store new secret in database
  put_query <- paste0("INSERT INTO keys_database (name, encrypted_data, description) VALUES ('", name, "', '", encrypted_data, "', '", description, "')")
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  DBI::dbExecute(db, put_query)
  DBI::dbDisconnect(db)
  
  cat("secret has been added")
  
}



#' custom_show_secrets
#'
#' encryps secret using master key and stores it in the local database
#' 
#' @param path_to_keys_db path to keys_database.sqlite
#' @return data data frame with names and descriptions
#'
#' @export
custom_show_secrets <- function(path_to_keys_db = "../../base-data/database/keys_database.sqlite") {
  
  # get secrets
  get_query <- "SELECT name, description FROM keys_database"
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  data <- DBI::dbGetQuery(db, get_query)
  DBI::dbDisconnect(db)
  
  return(data)
  
}



#' custom_delete_secret
#'
#' returns names of stored secrets and descriptions
#' 
#' @param name name of the new secret
#' @param path_to_keys_db path to keys_database.sqlite
#'
#' @export
custom_delete_secret <- function(secret, path_to_keys_db = "../../base-data/database/keys_database.sqlite") {
  
  put_query <- paste0("DELETE FROM keys_database WHERE name = '", secret, "'")
  db <- DBI::dbConnect(RSQLite::SQLite(), path_to_keys_db)
  DBI::dbExecute(db, put_query)
  DBI::dbDisconnect(db)
  
  cat("secret has been deleted")
  
}


#' custom_decrypt_data
#'
#' returns a dataframe with the decrypted content. The decryption key has to be passed down within a function.
#' 
#' @param decryption_key the secret to decrypt the data with
#' @param encrypted_df encrypted data frames
#'
#' @export
custom_decrypt_data <- function(decryption_key, encrypted_df) {
  
  encrypted_df %>% 
    safer::decrypt_object(decryption_key)

}



