
#' Check credentials
#'
#' @param db A \code{data.frame} with credentials data or path to SQLite database created with \code{\link{create_db}}.
#' @param passphrase Passphrase to decrypt the SQLite database.
#'
#' @return Return a \code{function} with two arguments: \code{user} and \code{password}
#' to be used in \code{\link{module-authentication}}. The authentication function returns
#' a \code{list} with 3 slots :
#'  \itemize{
#'   \item \strong{result} : logical, result of authentication.
#'   \item \strong{expired} : logical, is user has expired ? Always \code{FALSE} if \code{db} doesn't have a \code{expire} column.
#'   \item \strong{user_info} : the line in \code{db} corresponding to the user.
#'  }
#'
#'
#' @details The credentials \code{data.frame} can have the following columns:
#'  \itemize{
#'   \item \strong{user (mandatory)} : the user's name.
#'   \item \strong{password (mandatory)} : the user's password.
#'   \item \strong{admin (optional)} : logical, is user have admin right ? If so,
#'    user can access the admin mode (only available using a SQLite database)
#'   \item \strong{start (optional)} : the date from which the user will have access to the application
#'   \item \strong{expire (optional)} : the date from which the user will no longer have access to the application
#'   \item \strong{applications (optional)} : the name of the applications to which the user is authorized,
#'    separated by a semicolon. The name of the application corresponds to the name of the directory,
#'    or can be declared using : \code{options("shinymanager.application" = "my-app")}
#'   \item \strong{additional columns} : add others columns to retrieve the values server-side after authentication
#'  }
#'
#' @export
#'
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
#' \dontrun{
#'
#' ## With a SQLite database:
#'
#' check_credentials("credentials.sqlite", passphrase = "supersecret")
#'
#' }
#'
check_credentials <- function(db, passphrase = NULL) {
  if (is.data.frame(db)) {
    function(user, password) {
      check_credentials_df(user, password, credentials_df = db)
    }
  } else if (is_sqlite(db)) {
    .tok$set_sqlite_path(db)
    .tok$set_passphrase(passphrase)
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
      authorized = FALSE,
      user_info = NULL
    ))
  }
  user_info <- credentials_df[credentials_df$user == user, setdiff(names(credentials_df), "password"), drop = FALSE]
  pwd <- credentials_df$password[credentials_df$user == user]
  good_password <- isTRUE(pwd == password)
  if (hasName(credentials_df, "expire") | hasName(credentials_df, "start")) {
    if (is.null(user_info$start) | (!is.null(user_info$start) && is.na(user_info$start))) {
      user_info$start <- Sys.Date() - 1
    }
    if (is.null(user_info$expire) | (!is.null(user_info$expire) && is.na(user_info$expire))) {
      user_info$expire <- Sys.Date() + 1
    }
    good_time <- isTRUE(user_info$start <= Sys.Date() & user_info$expire >= Sys.Date())
  } else {
    good_time <- TRUE
  }
  authorized <- TRUE
  if (hasName(credentials_df, "applications")) {
    appname <- get_appname()
    appsnames <- credentials_df$applications[credentials_df$user == user]
    appsnames <- strsplit(x = as.character(appsnames), split = ";")
    appsnames <- unlist(x = appsnames, use.names = FALSE)
    if (!isTRUE(appname %in% appsnames)) {
      good_password <- FALSE
      authorized <- FALSE
    }
  }
  if (good_password) {
    if (good_time) {
      auth <- list(
        result = TRUE,
        expired = FALSE,
        authorized = authorized,
        user_info = user_info
      )
    } else {
      auth <- list(
        result = FALSE,
        expired = TRUE,
        authorized = authorized,
        user_info = user_info
      )
    }
  } else {
    auth <- list(
      result = FALSE,
      expired = FALSE,
      authorized = authorized,
      user_info = user_info
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



