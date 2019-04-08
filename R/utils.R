
is_sqlite <- function(path) {
  is.character(path) && file.exists(path) && grepl(pattern = "\\.sqlite$", x = path)
}

hasName <- function(x, name) {
  match(name, names(x), nomatch = 0L) > 0L
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
is_force_chg_pwd <- function(token, sqlite_path = NULL, passphrase = NULL) {
  user_info <- .tok$get(token)
  if (!is.null(sqlite_path)) {
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    resetpwd <- read_db_decrypt(conn, name = "resetpwd", passphrase = passphrase)
    ind_user <- user_info$user %in% resetpwd$user
    isTRUE(resetpwd$reset_pwd[ind_user])
  } else {
    return(FALSE)
  }
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
force_chg_pwd <- function(user, sqlite_path, passphrase) {
  conn <- dbConnect(SQLite(), dbname = sqlite_path)
  on.exit(dbDisconnect(conn))
  resetpwd <- read_db_decrypt(conn, name = "resetpwd", passphrase = passphrase)
  ind_user <- resetpwd$user %in% user
  resetpwd$reset_pwd[ind_user] <- TRUE
  write_db_encrypt(conn, value = resetpwd, name = "resetpwd", passphrase = passphrase)
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







