
#' Check credentials
#'
#' @param db A \code{data.frame} with credentials data or path to SQLite database..
#' @param passphrase Passphrase to decrypt the SQLite database.
#'
#' @return Return a \code{function} with two arguments: \code{user} and \code{password}
#' to be used in \code{\link{module-authentication}}. The authentication function returns
#' a \code{list} with 3 slots :
#'  \itemize{
#'   \item \strong{result} : logical, result of authentication.
#'   \item \strong{expired} : logical, is user has expired ? Always \code{FALSE} if \code{db} doesn't have a \code{expire} column.
#'   \item \strong{user_info} : the line in \code{credentials_df} corresponding to the user.
#'  }
#'
#' @export
#'
#' @importFrom utils hasName
#'
#'
#' @examples
#' # data.frame with credentials info
#' credentials <- data.frame(
#'   user = c("fanny", "victor"),
#'   password = c("azerty", "12345"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # check a user
#' check_credentials(credentials)("fanny", "azerty")
#' check_credentials(credentials)("fanny", "azert")
#' check_credentials(credentials)("fannyyy", "azerty")
#'
check_credentials <- function(db, passphrase = NULL) {
  if (is.data.frame(db)) {
    function(user, password) {
      check_credentials_df(user, password, credentials_df = db)
    }
  } else if (is_sqlite(db)) {
    check_credentials_sqlite(sqlite_path = db, passphrase = passphrase)
  } else {
    stop("'db' must be a data.frame or a path to a SQLite database", call. = FALSE)
  }
}


check_credentials_df <- function(user, password, credentials_df) {
  credentials_df <- as.data.frame(credentials_df)
  if (!user %in% credentials_df$user) {
    return(list(
      result = FALSE,
      expired = FALSE,
      user_info = NULL
    ))
  }
  user_info <- credentials_df[credentials_df$user == user, setdiff(names(credentials_df), "password"), drop = FALSE]
  pwd <- credentials_df$password[credentials_df$user == user]
  good_password <- isTRUE(pwd == password)
  if (hasName(credentials_df, "expire")) {
    if (is.null(user_info$start)) {
      user_info$start <- Sys.Date() - 1
    }
    good_time <- isTRUE(user_info$start <= Sys.Date() & user_info$expire >= Sys.Date())
  } else {
    good_time <- TRUE
  }
  if (good_password) {
    if (good_time) {
      auth <- list(
        result = TRUE,
        expired = FALSE,
        user_info = user_info
      )
    } else {
      auth <- list(
        result = FALSE,
        expired = TRUE,
        user_info = user_info
      )
    }
  } else {
    auth <- list(
      result = FALSE,
      expired = FALSE,
      user_info = NULL
    )
  }
  return(auth)
}

check_credentials_sqlite <- function(sqlite_path, passphrase) {
  conn <- dbConnect(SQLite(), dbname = sqlite_path)
  on.exit(dbDisconnect(conn))
  db <- read_db_decrypt(
    conn = conn,
    name = "credentials",
    passphrase = passphrase
  )
  function(user, password) {
    check_credentials_df(user, password, credentials_df = db)
  }
}


is_sqlite <- function(path) {
  is.character(path) && file.exists(path) && grepl(pattern = "\\.sqlite$", x = path)
}
