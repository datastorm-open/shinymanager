
# Self-service password reset from the authentication page.
#
# Reset the password of a user only if the provided email matches the one
# stored in the credentials table. A temporary password is generated and sent
# using the 'send_mail' callback configured in secure_server(). The new
# password is persisted only if the email has been sent successfully, so a
# failing mail server never locks the user out. The user is then forced to
# change the password on next login (must_change).
#
# Only available with a SQLite or SQL backend (the data.frame backend is not
# persistent). Returns a list with a single logical slot 'result'. Whatever the
# result, the caller always displays the same generic message to avoid account
# enumeration.

#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbExecute SQL
#' @importFrom RSQLite SQLite
#' @importFrom glue glue_sql
#' @importFrom scrypt hashPassword
reset_pwd_user_email <- function(user, email) {
  send_mail <- .tok$get_send_mail()
  if (!is.function(send_mail)) {
    return(list(result = FALSE))
  }
  if (is.null(user) || is.null(email) || !nzchar(trimws(user)) || !nzchar(trimws(email))) {
    return(list(result = FALSE))
  }

  user <- trimws(user)
  email <- trimws(email)

  email_col <- get_email_column()

  sqlite_path <- .tok$get_sqlite_path()
  passphrase  <- .tok$get_passphrase()
  config_db   <- .tok$get_sql_config_db()

  # sqlite
  if (!is.null(sqlite_path)) {
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))

    users <- read_db_decrypt(conn, name = "credentials", passphrase = passphrase)
    if (!email_col %in% colnames(users)) {
      return(list(result = FALSE))
    }
    ind_user <- which(users$user == user)
    if (length(ind_user) != 1) {
      return(list(result = FALSE))
    }
    stored_email <- as.character(users[[email_col]][ind_user])
    if (!identical(tolower(trimws(stored_email)), tolower(email))) {
      return(list(result = FALSE))
    }

    temp_pwd <- generate_pwd()

    # send email first: only persist the new password if the mail is sent
    sent <- tryCatch({
      send_mail(user, stored_email, temp_pwd)
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(sent)) {
      return(list(result = FALSE))
    }

    res_pwd <- try({
      if (!"character" %in% class(users$password)) {
        users$password <- as.character(users$password)
      }
      users$password[ind_user] <- temp_pwd
      if (!"is_hashed_password" %in% colnames(users)) {
        users$is_hashed_password <- FALSE
      }
      users$is_hashed_password[ind_user] <- FALSE
      write_db_encrypt(conn, value = users, name = "credentials", passphrase = passphrase)
      force_chg_pwd(user, TRUE)
    }, silent = TRUE)
    return(list(result = !inherits(res_pwd, "try-error")))

  } else if (!is.null(config_db)) {
    conn <- connect_sql_db(config_db)
    on.exit(disconnect_sql_db(conn, config_db))

    tablename <- SQL(config_db$tables$credentials$tablename)
    request <- glue_sql(config_db$tables$credentials$select, .con = conn)
    user_info <- dbGetQuery(conn, request)
    if (nrow(user_info) != 1 || !email_col %in% colnames(user_info)) {
      return(list(result = FALSE))
    }
    stored_email <- as.character(user_info[[email_col]][1])
    if (!identical(tolower(trimws(stored_email)), tolower(email))) {
      return(list(result = FALSE))
    }

    temp_pwd <- generate_pwd()

    sent <- tryCatch({
      send_mail(user, stored_email, temp_pwd)
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(sent)) {
      return(list(result = FALSE))
    }

    res_pwd <- try({
      value <- scrypt::hashPassword(temp_pwd)
      name <- "password"
      udpate_users <- user

      tablename <- SQL(config_db$tables$credentials$tablename)
      request <- glue_sql(config_db$tables$credentials$update, .con = conn)
      dbExecute(conn, request)

      force_chg_pwd(user, TRUE)
    }, silent = TRUE)
    return(list(result = !inherits(res_pwd, "try-error")))

  } else {
    return(list(result = FALSE))
  }
}
