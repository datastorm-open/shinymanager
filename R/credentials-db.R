
#' Create credentials database
#'
#' @param credentials_data A \code{data.frame}
#' @param sqlite_path Path to the SQLite database.
#' @param passphrase A password to protect the data inside the database.
#'
#' @export
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
create_db <- function(credentials_data, sqlite_path, passphrase = NULL) {
  if (!all(c("user", "password") %in% names(credentials_data))) {
    stop("credentials_data must contains columns: 'user', 'password'", call. = FALSE)
  }
  conn <- dbConnect(SQLite(), dbname = sqlite_path)
  on.exit(dbDisconnect(conn))
  credentials_data[] <- lapply(credentials_data, as.character)
  write_db_encrypt(
    conn = conn,
    name = "credentials",
    value = credentials_data,
    passphrase = passphrase
  )
  write_db_encrypt(
    conn = conn,
    name = "resetpwd",
    value = data.frame(
      user = credentials_data$user,
      reset_pwd = TRUE,
      stringsAsFactors = FALSE
    ),
    passphrase = passphrase
  )
}

#' @importFrom DBI dbWriteTable
#' @importFrom openssl sha256 aes_cbc_encrypt
write_db_encrypt <- function(conn, value, name = "credentials", passphrase = NULL) {
  if (!is.null(passphrase)) {
    passphrase <- as.character(passphrase)
    passphrase <- charToRaw(passphrase)
    key <- sha256(passphrase)
    value_serialized <- serialize(value, NULL)
    value_encrypted <- aes_cbc_encrypt(data = value_serialized, key = key)
    value <- data.frame(value = I(list(value_encrypted)), iv = I(list(attr(value_encrypted, "iv"))))
  }
  dbWriteTable(conn = conn, name = name, value = value, overwrite = TRUE)
}

#' @importFrom DBI dbReadTable
#' @importFrom openssl sha256 aes_cbc_decrypt
read_db_decrypt <- function(conn, name = "credentials", passphrase = NULL) {
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



#' Simple password generation
#'
#' @param n Number of password(s)
#'
#' @return a \code{character}
#' @export
#'
#' @importFrom openssl base64_encode rand_bytes
#'
#' @examples
#' generate_pwd()
#'
#' generate_pwd(3)
generate_pwd <- function(n = 1) {
  replicate(n = n, base64_encode(rand_bytes(6)))
}



