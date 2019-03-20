
#' Initialize credentials database
#'
#' @param credentials_data A \code{data.frame}
#' @param sqlite_path Path to the SQLite database.
#' @param passphrase A password to protect the data inside the database.
#'
#' @export
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
initialize_db <- function(credentials_data, sqlite_path, passphrase = NULL) {
  conn <- dbConnect(SQLite(), dbname = sqlite_path)
  on.exit(dbDisconnect(conn))
  credentials_data[] <- lapply(credentials_data, as.character)
  if (!is.null(passphrase)) {
    write_db_encrypt(
      conn = conn,
      name = "credentials",
      value = credentials_data,
      passphrase = passphrase
    )
  } else {
    dbWriteTable(
      conn = conn,
      name = "credentials",
      value = credentials_data
    )
  }
}

#' @importFrom DBI dbWriteTable
#' @importFrom openssl sha256 aes_cbc_encrypt
write_db_encrypt <- function(conn, value, passphrase, name = "credentials") {
  passphrase <- as.character(passphrase)
  passphrase <- charToRaw(passphrase)
  key <- sha256(passphrase)
  value_serialized <- serialize(value, NULL)
  value_encrypted <- aes_cbc_encrypt(data = value_serialized, key = key)
  value_db <- data.frame(value = I(list(value_encrypted)), iv = I(list(attr(value_encrypted, "iv"))))
  dbWriteTable(conn = conn, name = name, value = value_db)
}

#' @importFrom DBI dbReadTable
#' @importFrom openssl sha256 aes_cbc_decrypt
read_db_decrypt <- function(conn, name, passphrase = NULL) {
  out <- dbReadTable(conn = conn, name = name)
  if (!is.null(passphrase)) {
    passphrase <- as.character(passphrase)
    passphrase <- charToRaw(passphrase)
    key <- sha256(passphrase)
    value <- out$value[[1]]
    attr(value, "iv") <- out$iv[[1]]
    out <- aes_cbc_decrypt(value, key = key)
    out <- unserialize(out)
  }
  return(out)
}

