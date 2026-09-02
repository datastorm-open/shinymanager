
# Self-service password reset from the authentication page.
#
# Two modes, driven by options("shinymanager.reset_password"):
#  - TRUE        : the user must provide a username AND the email associated with
#                  it (the email is verified against the credentials table).
#  - "username"  : the user only provides a username; the temporary password is
#                  sent to whatever email is stored for that user (if any).
#
# In both cases a temporary password is generated and sent using the 'send_mail'
# callback configured in secure_server(). The new password is persisted only if
# the email has been sent successfully, so a failing mail server never locks the
# user out. The user is then forced to change the password on next login
# (must_change).
#
# Only available with a SQLite or SQL backend (the data.frame backend is not
# persistent). Returns a list with two slots:
#  - result : logical, TRUE if the password was reset.
#  - reason : a short server-side code describing the outcome, used ONLY for
#             admin-side logging (see save_reset_logs). It must never be shown to
#             the visitor: the caller always displays the same generic message to
#             avoid account enumeration.

# Decide, from the stored email, why a reset is (not) allowed.
# Returns "ok", "no_stored_email" or "email_mismatch".
email_reset_reason <- function(stored_email, email, require_email) {
  stored_email <- trimws(as.character(stored_email))
  if (length(stored_email) != 1 || is.na(stored_email) || !nzchar(stored_email)) {
    return("no_stored_email")
  }
  if (require_email && !identical(tolower(stored_email), tolower(email))) {
    return("email_mismatch")
  }
  "ok"
}

# Log a reset attempt to the admin logs, mapping the internal reason to a status.
# Only outcomes based on the submitted username / email are logged; technical
# cases (config, empty input, mail failure, db error) are skipped.
save_reset_logs <- function(user, reason) {
  if (!write_logs_enabled()) {
    return(invisible())
  }
  status <- switch(
    reason,
    "success"         = "Reset password",
    "unknown_user"    = "Reset password: unknown user",
    "email_mismatch"  = "Reset password: wrong email",
    "no_stored_email" = "Reset password: no email",
    NULL
  )
  if (!is.null(status)) {
    save_logs_failed(user, status = status)
  }
  invisible()
}

#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbExecute SQL
#' @importFrom RSQLite SQLite
#' @importFrom glue glue_sql
#' @importFrom scrypt hashPassword
reset_pwd_user_email <- function(user, email = NULL) {
  send_mail <- .tok$get_send_mail()
  if (!is.function(send_mail)) {
    return(list(result = FALSE, reason = "no_mailer"))
  }
  if (is.null(user) || !nzchar(trimws(user))) {
    return(list(result = FALSE, reason = "empty_input"))
  }

  require_email <- !reset_password_username_only()
  if (require_email && (is.null(email) || !nzchar(trimws(email)))) {
    return(list(result = FALSE, reason = "empty_input"))
  }

  user <- trimws(user)
  email <- if (is.null(email)) "" else trimws(email)

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
      return(list(result = FALSE, reason = "no_email_column"))
    }
    ind_user <- which(users$user == user)
    if (length(ind_user) != 1) {
      return(list(result = FALSE, reason = "unknown_user"))
    }
    stored_email <- as.character(users[[email_col]][ind_user])
    reason <- email_reset_reason(stored_email, email, require_email)
    if (reason != "ok") {
      return(list(result = FALSE, reason = reason))
    }

    temp_pwd <- generate_pwd()

    # send email first: only persist the new password if the mail is sent
    sent <- tryCatch({
      send_mail(user, stored_email, temp_pwd)
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(sent)) {
      return(list(result = FALSE, reason = "mail_failed"))
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
    ok <- !inherits(res_pwd, "try-error")
    return(list(result = ok, reason = if (ok) "success" else "db_error"))

  } else if (!is.null(config_db)) {
    conn <- connect_sql_db(config_db)
    on.exit(disconnect_sql_db(conn, config_db))

    tablename <- SQL(config_db$tables$credentials$tablename)
    request <- glue_sql(config_db$tables$credentials$select, .con = conn)
    user_info <- dbGetQuery(conn, request)
    if (nrow(user_info) != 1) {
      return(list(result = FALSE, reason = "unknown_user"))
    }
    if (!email_col %in% colnames(user_info)) {
      return(list(result = FALSE, reason = "no_email_column"))
    }
    stored_email <- as.character(user_info[[email_col]][1])
    reason <- email_reset_reason(stored_email, email, require_email)
    if (reason != "ok") {
      return(list(result = FALSE, reason = reason))
    }

    temp_pwd <- generate_pwd()

    sent <- tryCatch({
      send_mail(user, stored_email, temp_pwd)
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(sent)) {
      return(list(result = FALSE, reason = "mail_failed"))
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
    ok <- !inherits(res_pwd, "try-error")
    return(list(result = ok, reason = if (ok) "success" else "db_error"))

  } else {
    return(list(result = FALSE, reason = "backend"))
  }
}
