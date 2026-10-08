context("test-reset-password")

tmp_sqlite <- tempfile(fileext = ".sqlite")

credentials <- data.frame(
  user = c("fanny", "victor"),
  password = c("azerty12", "12345A"),
  email = c("fanny@mail.com", "victor@mail.com"),
  stringsAsFactors = FALSE
)

create_db(credentials_data = credentials, sqlite_path = tmp_sqlite, passphrase = "secret")

# point the token store to our test database, as check_credentials() would
.tok$set_sqlite_path(tmp_sqlite)
.tok$set_passphrase("secret")
.tok$set_sql_config_db(NULL)

# fake mailer capturing what would be sent, no SMTP involved
captured <- new.env()
reset_capture <- function() {
  captured$called <- 0L
  captured$user <- NULL
  captured$email <- NULL
  captured$pwd <- NULL
}
fake_mail_ok <- function(user, email, temp_password) {
  captured$called <- captured$called + 1L
  captured$user <- user
  captured$email <- email
  captured$pwd <- temp_password
  invisible(TRUE)
}


test_that("option helpers read their options", {
  old <- options(shinymanager.reset_password = TRUE, shinymanager.email_column = "mail")
  expect_true(reset_password_enabled())
  expect_equal(get_email_column(), "mail")
  options(old)
})

test_that("get_email_column defaults to 'email'", {
  old <- options(shinymanager.email_column = NULL)
  expect_equal(get_email_column(), "email")
  options(old)
})

test_that("reset succeeds with valid user + email, mails a temp password", {
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)

  res <- reset_pwd_user_email("fanny", "fanny@mail.com")

  expect_true(res$result)
  expect_equal(captured$called, 1L)
  expect_equal(captured$user, "fanny")
  expect_equal(captured$email, "fanny@mail.com")
  expect_true(is.character(captured$pwd) && nchar(captured$pwd) > 0)

  # the stored (hashed) password now matches the temporary one that was mailed
  db <- read_db_decrypt(conn = tmp_sqlite, name = "credentials", passphrase = "secret")
  stored <- db$password[db$user == "fanny"]
  expect_true(scrypt::verifyPassword(stored, captured$pwd))

  # user must change the password on next login
  pm <- read_db_decrypt(conn = tmp_sqlite, name = "pwd_mngt", passphrase = "secret")
  expect_true(isTRUE(as.logical(pm$must_change[pm$user == "fanny"])))
})

test_that("email match is case-insensitive and trimmed", {
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)
  res <- reset_pwd_user_email("victor", "  VICTOR@MAIL.COM ")
  expect_true(res$result)
  expect_equal(captured$called, 1L)
})

test_that("reset refused with wrong email, nothing sent nor changed", {
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)

  db_before <- read_db_decrypt(tmp_sqlite, "credentials", "secret")
  pwd_before <- db_before$password[db_before$user == "fanny"]

  res <- reset_pwd_user_email("fanny", "not-the-right@mail.com")

  expect_false(res$result)
  expect_equal(captured$called, 0L)

  db_after <- read_db_decrypt(tmp_sqlite, "credentials", "secret")
  expect_identical(db_after$password[db_after$user == "fanny"], pwd_before)
})

test_that("reset refused for unknown user", {
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)
  res <- reset_pwd_user_email("ghost", "ghost@mail.com")
  expect_false(res$result)
  expect_equal(captured$called, 0L)
})

test_that("password is NOT reset if mail sending fails (no lockout)", {
  .tok$set_send_mail(function(user, email, temp_password) stop("smtp down"))

  db_before <- read_db_decrypt(tmp_sqlite, "credentials", "secret")
  pwd_before <- db_before$password[db_before$user == "victor"]

  # the SMTP error is reported as a warning
  expect_warning(
    res <- reset_pwd_user_email("victor", "victor@mail.com"),
    "smtp down"
  )

  expect_false(res$result)
  db_after <- read_db_decrypt(tmp_sqlite, "credentials", "secret")
  expect_identical(db_after$password[db_after$user == "victor"], pwd_before)
})

test_that("no reset when send_mail is not configured", {
  .tok$set_send_mail(NULL)
  expect_warning(res <- reset_pwd_user_email("fanny", "fanny@mail.com"), "send_mail")
  expect_false(res$result)
})

test_that("no reset when the email column is missing", {
  tmp2 <- tempfile(fileext = ".sqlite")
  create_db(
    credentials_data = data.frame(user = "alice", password = "azerty12", stringsAsFactors = FALSE),
    sqlite_path = tmp2, passphrase = "secret"
  )
  .tok$set_sqlite_path(tmp2)
  .tok$set_send_mail(fake_mail_ok)
  reset_capture()

  expect_warning(res <- reset_pwd_user_email("alice", "alice@mail.com"), "email")
  expect_false(res$result)
  expect_equal(captured$called, 0L)

  .tok$set_sqlite_path(tmp_sqlite) # restore for any later use
})

test_that("custom email column name is honored", {
  tmp3 <- tempfile(fileext = ".sqlite")
  create_db(
    credentials_data = data.frame(
      user = "bob", password = "azerty12", mail = "bob@mail.com",
      stringsAsFactors = FALSE
    ),
    sqlite_path = tmp3, passphrase = "secret"
  )
  old <- options(shinymanager.email_column = "mail")

  .tok$set_sqlite_path(tmp3)
  .tok$set_send_mail(fake_mail_ok)
  reset_capture()

  res <- reset_pwd_user_email("bob", "bob@mail.com")
  expect_true(res$result)
  expect_equal(captured$called, 1L)

  options(old)
  .tok$set_sqlite_path(tmp_sqlite) # restore
})

test_that("reset_password_username_only follows the option", {
  old <- options(shinymanager.reset_password = "username")
  expect_true(reset_password_enabled())
  expect_true(reset_password_username_only())
  options(shinymanager.reset_password = TRUE)
  expect_true(reset_password_enabled())
  expect_false(reset_password_username_only())
  options(old)
})

test_that("username-only mode: reset works without email input", {
  old <- options(shinymanager.reset_password = "username")
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)

  res <- reset_pwd_user_email("fanny")  # no email provided

  expect_true(res$result)
  expect_equal(captured$called, 1L)
  # temp password is sent to the email stored in the database
  expect_equal(captured$email, "fanny@mail.com")

  db <- read_db_decrypt(conn = tmp_sqlite, name = "credentials", passphrase = "secret")
  stored <- db$password[db$user == "fanny"]
  expect_true(scrypt::verifyPassword(stored, captured$pwd))

  options(old)
})

test_that("username-only mode: unknown user is refused, nothing sent", {
  old <- options(shinymanager.reset_password = "username")
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)
  expect_false(reset_pwd_user_email("ghost")$result)
  expect_equal(captured$called, 0L)
  options(old)
})

test_that("username-only mode: refused if user has no stored email", {
  tmp4 <- tempfile(fileext = ".sqlite")
  create_db(
    credentials_data = data.frame(
      user = "noemail", password = "azerty12", email = NA_character_,
      stringsAsFactors = FALSE
    ),
    sqlite_path = tmp4, passphrase = "secret"
  )
  old <- options(shinymanager.reset_password = "username")
  .tok$set_sqlite_path(tmp4)
  .tok$set_send_mail(fake_mail_ok)
  reset_capture()

  expect_false(reset_pwd_user_email("noemail")$result)
  expect_equal(captured$called, 0L)

  options(old)
  .tok$set_sqlite_path(tmp_sqlite) # restore
})

test_that("reset_pwd_user_email returns granular reasons", {
  .tok$set_sqlite_path(tmp_sqlite)
  .tok$set_send_mail(fake_mail_ok)
  expect_equal(reset_pwd_user_email("ghost", "x@mail.com")$reason, "unknown_user")
  expect_equal(reset_pwd_user_email("fanny", "wrong@mail.com")$reason, "email_mismatch")
  expect_equal(reset_pwd_user_email("fanny", "fanny@mail.com")$reason, "success")
})

test_that("reason is mail_failed when send_mail throws", {
  .tok$set_send_mail(function(user, email, temp_password) stop("smtp down"))
  expect_warning(res <- reset_pwd_user_email("victor", "victor@mail.com"), "smtp down")
  expect_equal(res$reason, "mail_failed")
})

test_that("reason is no_mailer when send_mail is not configured", {
  .tok$set_send_mail(NULL)
  expect_warning(res <- reset_pwd_user_email("fanny", "fanny@mail.com"), "send_mail")
  expect_equal(res$reason, "no_mailer")
})

test_that("username mode: reason is no_stored_email when user has no email", {
  tmp5 <- tempfile(fileext = ".sqlite")
  create_db(
    credentials_data = data.frame(
      user = "ne", password = "azerty12", email = NA_character_, stringsAsFactors = FALSE
    ),
    sqlite_path = tmp5, passphrase = "secret"
  )
  old <- options(shinymanager.reset_password = "username")
  .tok$set_sqlite_path(tmp5)
  .tok$set_send_mail(fake_mail_ok)
  expect_equal(reset_pwd_user_email("ne")$reason, "no_stored_email")
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("save_reset_logs writes a mapped status, skips empty input", {
  .tok$set_sqlite_path(tmp_sqlite)

  before <- read_db_decrypt(tmp_sqlite, "logs", "secret")
  save_reset_logs("fanny", "success")
  after <- read_db_decrypt(tmp_sqlite, "logs", "secret")
  expect_equal(nrow(after), nrow(before) + 1)
  expect_equal(after$status[nrow(after)], "Reset password")

  save_reset_logs("someone", "unknown_user")
  logs2 <- read_db_decrypt(tmp_sqlite, "logs", "secret")
  expect_equal(logs2$status[nrow(logs2)], "Reset password: unknown user")

  # errors are logged too
  save_reset_logs("fanny", "mail_failed")
  save_reset_logs("fanny", "no_mailer")
  logs3 <- read_db_decrypt(tmp_sqlite, "logs", "secret")
  expect_equal(tail(logs3$status, 2), c("Reset password: mail failed", "Reset password: config error"))

  # an empty input must not create a log line
  n <- nrow(logs3)
  save_reset_logs("fanny", "empty_input")
  expect_equal(nrow(read_db_decrypt(tmp_sqlite, "logs", "secret")), n)
})

test_that("get_reset_password_validity reads the option, NA when unset or invalid", {
  old <- options(shinymanager.reset_password_validity = NULL)
  expect_true(is.na(get_reset_password_validity()))
  options(shinymanager.reset_password_validity = 20)
  expect_equal(get_reset_password_validity(), 20)
  options(shinymanager.reset_password_validity = -5)
  expect_true(is.na(get_reset_password_validity()))
  options(shinymanager.reset_password_validity = "abc")
  expect_true(is.na(get_reset_password_validity()))
  options(old)
})

test_that("without validity option, no expiration column is created", {
  tmp6 <- tempfile(fileext = ".sqlite")
  create_db(credentials_data = credentials, sqlite_path = tmp6, passphrase = "secret")
  old <- options(shinymanager.reset_password_validity = NULL)
  .tok$set_sqlite_path(tmp6)
  .tok$set_send_mail(fake_mail_ok)

  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  pm <- read_db_decrypt(tmp6, "pwd_mngt", "secret")
  expect_false("temp_pwd_expire" %in% colnames(pm))
  expect_false(is_temp_pwd_expired("fanny"))

  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("with validity option, the mailed password gets an expiration", {
  tmp7 <- tempfile(fileext = ".sqlite")
  create_db(credentials_data = credentials, sqlite_path = tmp7, passphrase = "secret")
  old <- options(shinymanager.reset_password_validity = 20)
  .tok$set_sqlite_path(tmp7)
  .tok$set_send_mail(fake_mail_ok)

  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  pm <- read_db_decrypt(tmp7, "pwd_mngt", "secret")
  expire <- as.POSIXct(pm$temp_pwd_expire[pm$user == "fanny"], tz = "UTC")
  expect_true(expire > Sys.time() + 19 * 60 && expire <= Sys.time() + 20 * 60)
  # other users are not concerned
  expect_equal(pm$temp_pwd_expire[pm$user == "victor"], "")
  expect_false(is_temp_pwd_expired("fanny"))
  expect_false(is_temp_pwd_expired("victor"))

  # once the validity has passed, the temporary password is expired
  set_temp_pwd_expire("fanny", "2000-01-01 00:00:00")
  expect_true(is_temp_pwd_expired("fanny"))

  # a new request replaces the previous expiration
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_false(is_temp_pwd_expired("fanny"))

  # a new request without the option removes the expiration
  set_temp_pwd_expire("fanny", "2000-01-01 00:00:00")
  options(shinymanager.reset_password_validity = NULL)
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_false(is_temp_pwd_expired("fanny"))

  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("changing the password (or an admin reset) clears the expiration", {
  tmp8 <- tempfile(fileext = ".sqlite")
  create_db(credentials_data = credentials, sqlite_path = tmp8, passphrase = "secret")
  .tok$set_sqlite_path(tmp8)

  set_temp_pwd_expire("fanny", "2000-01-01 00:00:00")
  expect_true(is_temp_pwd_expired("fanny"))
  expect_true(update_pwd("fanny", "NewPassword1")$result)
  pm <- read_db_decrypt(tmp8, "pwd_mngt", "secret")
  expect_equal(pm$temp_pwd_expire[pm$user == "fanny"], "")
  expect_false(is_temp_pwd_expired("fanny"))

  # admin reset path: clearing
  set_temp_pwd_expire("victor", "2000-01-01 00:00:00")
  set_temp_pwd_expire("victor", "")
  expect_false(is_temp_pwd_expired("victor"))

  .tok$set_sqlite_path(tmp_sqlite)
})

# fresh database, set as the current one
new_test_db <- function(credentials_data = credentials) {
  tmp <- tempfile(fileext = ".sqlite")
  create_db(credentials_data = credentials_data, sqlite_path = tmp, passphrase = "secret")
  .tok$set_sqlite_path(tmp)
  # forget the in-memory reset times (server-side cooldown) of previous tests
  .tok$.__enclos_env__$private$reset_times <- list()
  tmp
}

test_that("get_reset_password_max reads the option, Inf when unset or invalid", {
  old <- options(shinymanager.reset_password_max = NULL)
  expect_equal(get_reset_password_max(), Inf)
  options(shinymanager.reset_password_max = 3)
  expect_equal(get_reset_password_max(), 3)
  options(shinymanager.reset_password_max = 0)
  expect_equal(get_reset_password_max(), Inf)
  options(shinymanager.reset_password_max = "abc")
  expect_equal(get_reset_password_max(), Inf)
  options(old)
})

test_that("without max option, no reset counter column is created", {
  tmp <- new_test_db()
  old <- options(shinymanager.reset_password_max = NULL)
  .tok$set_send_mail(fake_mail_ok)
  for (i in 1:3) expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_false("n_user_reset" %in% colnames(read_db_decrypt(tmp, "pwd_mngt", "secret")))
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("with max option, resets are limited until a successful login", {
  tmp <- new_test_db()
  old <- options(shinymanager.reset_password_max = 2)
  .tok$set_send_mail(fake_mail_ok)

  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_equal(get_n_user_reset("fanny"), 2)

  reset_capture()
  res <- reset_pwd_user_email("fanny", "fanny@mail.com")
  expect_false(res$result)
  expect_equal(res$reason, "too_many_resets")
  expect_equal(captured$called, 0L)
  # other users are not concerned
  expect_true(reset_pwd_user_email("victor", "victor@mail.com")$result)

  # successful login with a non temporary password
  reset_login_counters("fanny")
  expect_equal(get_n_user_reset("fanny"), 0)
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)

  # a failed sending is not counted
  .tok$set_send_mail(function(user, email, temp_password) stop("smtp down"))
  expect_warning(reset_pwd_user_email("fanny", "fanny@mail.com"), "smtp down")
  expect_equal(get_n_user_reset("fanny"), 1)

  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("a locked account is unlocked by a reset, unless reset_password_unlock = FALSE", {
  tmp <- new_test_db()
  old <- options(shinymanager.pwd_failure_limit = 2, shinymanager.reset_password_unlock = NULL)
  .tok$set_send_mail(fake_mail_ok)
  save_logs_failed("fanny", status = "Wrong pwd")
  save_logs_failed("fanny", status = "Wrong pwd")
  expect_true(check_locked_account("fanny", 2))

  # default: the reset unlocks the account
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_false(check_locked_account("fanny", 2))

  # admin only
  options(shinymanager.reset_password_unlock = FALSE)
  save_logs_failed("fanny", status = "Wrong pwd")
  save_logs_failed("fanny", status = "Wrong pwd")
  reset_capture()
  res <- reset_pwd_user_email("fanny", "fanny@mail.com")
  expect_equal(res$reason, "locked_account")
  expect_equal(captured$called, 0L)
  expect_true(check_locked_account("fanny", 2))
  # not locked: reset still possible
  expect_true(reset_pwd_user_email("victor", "victor@mail.com")$result)

  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("no reset for an expired or unauthorized account", {
  old <- options(shinymanager.application = "myapp")
  tmp <- new_test_db(data.frame(
    user = c("old", "other", "ok"),
    password = "azerty12",
    email = c("old@mail.com", "other@mail.com", "ok@mail.com"),
    expire = c("2000-01-01", NA, NA),
    applications = c("myapp", "another_app", "myapp;another_app"),
    stringsAsFactors = FALSE
  ))
  .tok$set_send_mail(fake_mail_ok)
  reset_capture()
  expect_equal(reset_pwd_user_email("old", "old@mail.com")$reason, "account_expired")
  expect_equal(reset_pwd_user_email("other", "other@mail.com")$reason, "unauthorized")
  expect_equal(captured$called, 0L)
  expect_true(reset_pwd_user_email("ok", "ok@mail.com")$result)
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("save_logs_failed respects write_logs but still counts wrong passwords", {
  tmp <- new_test_db()
  old <- options(shinymanager.write_logs = FALSE)
  save_logs_failed("fanny", status = "Wrong pwd")
  expect_equal(nrow(read_db_decrypt(tmp, "logs", "secret")), 0)
  pm <- read_db_decrypt(tmp, "pwd_mngt", "secret")
  expect_equal(pm$n_wrong_pwd[pm$user == "fanny"], 1)
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("old database schema (no status, n_wrong_pwd, optional columns) still works", {
  tmp <- new_test_db()
  # old logs without status, old pwd_mngt without n_wrong_pwd
  logs <- read_db_decrypt(tmp, "logs", "secret")
  logs <- rbind(logs, data.frame(
    user = "fanny", server_connected = as.character(Sys.time()), token = "tk",
    logout = NA_character_, app = "app", stringsAsFactors = FALSE
  ))
  write_db_encrypt(tmp, value = logs, name = "logs", passphrase = "secret")
  pm <- read_db_decrypt(tmp, "pwd_mngt", "secret")
  pm$n_wrong_pwd <- NULL
  write_db_encrypt(tmp, value = pm, name = "pwd_mngt", passphrase = "secret")

  old <- options(shinymanager.pwd_failure_limit = 3, shinymanager.reset_password_max = NULL,
                 shinymanager.reset_password_validity = NULL)
  expect_error(reset_login_counters("fanny"), NA)
  expect_false(check_locked_account("fanny", 3))
  expect_equal(get_n_user_reset("fanny"), 0)
  expect_false(is_temp_pwd_expired("fanny"))
  expect_error(save_logs_failed("fanny", status = "Wrong pwd"), NA)
  .tok$set_send_mail(fake_mail_ok)
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)

  # nothing added that was not asked for
  pm <- read_db_decrypt(tmp, "pwd_mngt", "secret")
  expect_false(any(c("n_user_reset", "temp_pwd_expire") %in% colnames(pm)))

  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("SQLite credentials checker sees a reset done after its creation", {
  tmp <- new_test_db()
  checker <- check_credentials(tmp, passphrase = "secret")
  expect_true(checker("fanny", "azerty12")$result)

  reset_capture()
  .tok$set_send_mail(fake_mail_ok)
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_true(checker("fanny", captured$pwd)$result)
  expect_false(checker("fanny", "azerty12")$result)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("a failed save leaves password and pwd_mngt untouched (transaction)", {
  tmp <- new_test_db()
  old <- options(shinymanager.reset_password_max = 3, shinymanager.reset_password_validity = 20)
  .tok$set_send_mail(fake_mail_ok)
  db_before <- read_db_decrypt(tmp, "credentials", "secret")
  pm_before <- read_db_decrypt(tmp, "pwd_mngt", "secret")

  # pwd_mngt write fails, after the credentials write
  real_write <- write_db_encrypt
  local_mocked_bindings(write_db_encrypt = function(conn, value, name = "credentials", passphrase = NULL) {
    if (name == "pwd_mngt") stop("disk full")
    real_write(conn, value, name, passphrase)
  })
  expect_warning(res <- reset_pwd_user_email("fanny", "fanny@mail.com"), "disk full")
  expect_equal(res$reason, "db_error")

  expect_identical(read_db_decrypt(tmp, "credentials", "secret"), db_before)
  expect_identical(read_db_decrypt(tmp, "pwd_mngt", "secret"), pm_before)
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("a change made while sending the email is not overwritten", {
  tmp <- new_test_db()
  # another session updates victor while fanny's email is being sent
  .tok$set_send_mail(function(user, email, temp_password) {
    users <- read_db_decrypt(tmp, "credentials", "secret")
    users$email[users$user == "victor"] <- "new-victor@mail.com"
    write_db_encrypt(tmp, value = users, name = "credentials", passphrase = "secret")
  })
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  users <- read_db_decrypt(tmp, "credentials", "secret")
  expect_equal(users$email[users$user == "victor"], "new-victor@mail.com")
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("the account modified while sending the email is not overwritten", {
  tmp <- new_test_db()
  # an admin changes fanny's email and password while the email is being sent
  .tok$set_send_mail(function(user, email, temp_password) {
    users <- read_db_decrypt(tmp, "credentials", "secret")
    users$email[users$user == "fanny"] <- "new-fanny@mail.com"
    users$password[users$user == "fanny"] <- "admin-pwd"
    write_db_encrypt(tmp, value = users, name = "credentials", passphrase = "secret")
  })
  expect_warning(res <- reset_pwd_user_email("fanny", "fanny@mail.com"), "modified")
  expect_equal(res$reason, "db_error")
  users <- read_db_decrypt(tmp, "credentials", "secret")
  expect_equal(users$password[users$user == "fanny"], "admin-pwd")
  expect_false(isTRUE(as.logical(read_db_decrypt(tmp, "pwd_mngt", "secret")$must_change[1])))
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("the stored email is trimmed before sending", {
  cred <- credentials
  cred$email[cred$user == "fanny"] <- "  fanny@mail.com "
  tmp <- new_test_db(cred)
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_equal(captured$email, "fanny@mail.com")
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("a NA wrong password counter is counted from 0", {
  tmp <- new_test_db()
  pm <- read_db_decrypt(tmp, "pwd_mngt", "secret")
  pm$n_wrong_pwd <- NA
  write_db_encrypt(tmp, value = pm, name = "pwd_mngt", passphrase = "secret")
  save_logs_failed("fanny", status = "Wrong pwd")
  pm <- read_db_decrypt(tmp, "pwd_mngt", "secret")
  expect_equal(pm$n_wrong_pwd[pm$user == "fanny"], 1)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("get_reset_password_cooldown reads the option, 60 when unset or invalid", {
  old <- options(shinymanager.reset_password_cooldown = NULL)
  on.exit(options(old), add = TRUE)
  expect_equal(get_reset_password_cooldown(), 60)
  options(shinymanager.reset_password_cooldown = 30)
  expect_equal(get_reset_password_cooldown(), 30)
  options(shinymanager.reset_password_cooldown = 0)
  expect_equal(get_reset_password_cooldown(), 0)
  options(shinymanager.reset_password_cooldown = -1)
  expect_equal(get_reset_password_cooldown(), 60)
  options(shinymanager.reset_password_cooldown = "abc")
  expect_equal(get_reset_password_cooldown(), 60)
})

test_that("auth module: no cooldown with the option set to 0", {
  tmp <- new_test_db()
  .tok$set_send_mail(fake_mail_ok)
  checker <- check_credentials(tmp, passphrase = "secret")
  srv <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      auth_server(input, output, session, check_credentials = checker)
    })
  }
  old <- options(shinymanager.reset_password = TRUE, shinymanager.reset_password_cooldown = 0)
  on.exit(options(old), add = TRUE)
  reset_capture()
  shiny::testServer(srv, args = list(id = "auth"), {
    session$setInputs(reset_user = "fanny", reset_email = "fanny@mail.com", do_reset_pwd = 1)
    session$setInputs(do_reset_pwd = 2)
    session$setInputs(do_reset_pwd = 3)
    expect_equal(captured$called, 2L)
  })
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("tokens: the reset cooldown is per user", {
  tmp <- new_test_db()
  expect_false(.tok$is_reset_too_soon("fanny", 60))
  .tok$set_reset_time("fanny")
  expect_true(.tok$is_reset_too_soon("fanny", 60))
  expect_false(.tok$is_reset_too_soon("fanny", 0))
  expect_false(.tok$is_reset_too_soon("victor", 60))
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("auth module: no second email for the same user before the cooldown", {
  tmp <- new_test_db()
  .tok$set_send_mail(fake_mail_ok)
  checker <- check_credentials(tmp, passphrase = "secret")
  srv <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      auth_server(input, output, session, check_credentials = checker)
    })
  }
  old <- options(shinymanager.reset_password = TRUE)
  reset_capture()
  shiny::testServer(srv, args = list(id = "auth"), {
    session$setInputs(reset_user = "fanny", reset_email = "fanny@mail.com", do_reset_pwd = 1)
    session$setInputs(do_reset_pwd = 2)
    # sent directly to the server, without waiting for the button
    session$setInputs(do_reset_pwd = 3)
    expect_equal(captured$called, 1L)
  })
  # another session (shared in-memory times): still no email
  shiny::testServer(srv, args = list(id = "auth"), {
    session$setInputs(reset_user = " fanny ", reset_email = "fanny@mail.com", do_reset_pwd = 1)
    session$setInputs(do_reset_pwd = 2)
    expect_equal(captured$called, 1L)
  })
  logs <- read_db_decrypt(tmp, "logs", "secret")
  expect_equal(sum(logs$status %in% "Reset password: too soon"), 2)
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("auth module: no reset when disabled, login with the mailed password otherwise", {
  tmp <- new_test_db()
  .tok$set_send_mail(fake_mail_ok)
  checker <- check_credentials(tmp, passphrase = "secret")
  srv <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      auth_server(input, output, session, check_credentials = checker)
    })
  }

  old <- options(shinymanager.reset_password = FALSE)
  reset_capture()
  shiny::testServer(srv, args = list(id = "auth"), {
    session$setInputs(reset_user = "fanny", reset_email = "fanny@mail.com", do_reset_pwd = 1)
    # the first value is ignored (ignoreInit), as for a real click
    session$setInputs(do_reset_pwd = 2)
    expect_equal(captured$called, 0L)
  })

  options(shinymanager.reset_password = TRUE)
  shiny::testServer(srv, args = list(id = "auth"), {
    session$setInputs(reset_user = "fanny", reset_email = "fanny@mail.com", do_reset_pwd = 1)
    session$setInputs(do_reset_pwd = 2)
    expect_equal(captured$called, 1L)
    # same session: the mailed password is accepted
    session$setInputs(user_id = "fanny", user_pwd = captured$pwd, go_auth = 1)
    expect_true(session$returned$result)
  })
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("transaction: an error of dbBegin stops, except for backends without transactions", {
  tmp <- new_test_db()
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = tmp)
  on.exit(DBI::dbDisconnect(conn))
  called <- FALSE
  local_mocked_bindings(dbBegin = function(conn, ...) stop("database is locked"))
  expect_error(run_in_transaction(conn, function() called <<- TRUE), "locked")
  expect_false(called)

  # Spark / BigQuery: no transaction, dbBegin is not called
  fake_spark <- structure(list(), class = "spark_connection")
  expect_true(run_in_transaction(fake_spark, function() TRUE))
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("no pwd_mngt row: no email sent and password unchanged", {
  tmp <- new_test_db()
  pm <- read_db_decrypt(tmp, "pwd_mngt", "secret")
  write_db_encrypt(tmp, value = pm[pm$user != "fanny", ], name = "pwd_mngt", passphrase = "secret")
  db_before <- read_db_decrypt(tmp, "credentials", "secret")
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)

  expect_warning(res <- reset_pwd_user_email("fanny", "fanny@mail.com"), "pwd_mngt")
  expect_equal(res$reason, "db_error")
  expect_equal(captured$called, 0L)
  expect_identical(read_db_decrypt(tmp, "credentials", "secret"), db_before)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("a timestamp 'temp_pwd_expire' column is refused", {
  pm <- data.frame(user = "fanny", temp_pwd_expire = Sys.time())
  expect_error(check_reset_pwd_mngt(pm), "text column")
  pm$temp_pwd_expire <- ""
  expect_true(check_reset_pwd_mngt(pm))
  expect_error(check_reset_pwd_mngt(pm[0, ]), "pwd_mngt")
})

test_that("auth module: a database error during a reset is logged, the session goes on", {
  tmp <- new_test_db()
  .tok$set_send_mail(fake_mail_ok)
  checker <- check_credentials(tmp, passphrase = "secret")
  srv <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      auth_server(input, output, session, check_credentials = checker)
    })
  }
  old <- options(shinymanager.reset_password = TRUE)
  local_mocked_bindings(reset_pwd_user_email = function(user, email = NULL) stop("connection lost"))
  shiny::testServer(srv, args = list(id = "auth"), {
    session$setInputs(reset_user = "fanny", reset_email = "fanny@mail.com", do_reset_pwd = 1)
    expect_warning(session$setInputs(do_reset_pwd = 2), "connection lost")
    # login still works in the same session
    session$setInputs(user_id = "fanny", user_pwd = "azerty12", go_auth = 1)
    session$setInputs(go_auth = 2)
    expect_true(session$returned$result)
  })
  logs <- read_db_decrypt(tmp, "logs", "secret")
  expect_true("Reset password: db error" %in% logs$status)
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("auth module: an expired temporary password is refused at login", {
  tmp <- new_test_db()
  old <- options(shinymanager.reset_password = TRUE, shinymanager.reset_password_validity = 20)
  reset_capture()
  .tok$set_send_mail(fake_mail_ok)
  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  set_temp_pwd_expire("fanny", "2000-01-01 00:00:00")

  checker <- check_credentials(tmp, passphrase = "secret")
  srv <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      auth_server(input, output, session, check_credentials = checker)
    })
  }
  shiny::testServer(srv, args = list(id = "auth"), {
    session$setInputs(user_id = "fanny", user_pwd = captured$pwd, go_auth = 1)
    session$setInputs(go_auth = 2)
    expect_false(session$returned$result)
  })
  logs <- read_db_decrypt(tmp, "logs", "secret")
  expect_true("Reset password: expired" %in% logs$status)
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

test_that("auth module: empty fields, nothing sent nor logged", {
  tmp <- new_test_db()
  .tok$set_send_mail(fake_mail_ok)
  checker <- check_credentials(tmp, passphrase = "secret")
  srv <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      auth_server(input, output, session, check_credentials = checker)
    })
  }
  old <- options(shinymanager.reset_password = TRUE)
  reset_capture()
  shiny::testServer(srv, args = list(id = "auth"), {
    session$setInputs(reset_user = "", reset_email = "", do_reset_pwd = 1)
    session$setInputs(do_reset_pwd = 2)
    session$setInputs(reset_user = "fanny", reset_email = " ", do_reset_pwd = 3)
  })
  expect_equal(captured$called, 0L)
  expect_equal(nrow(read_db_decrypt(tmp, "logs", "secret")), 0)
  options(old)
  .tok$set_sqlite_path(tmp_sqlite)
})

# reset the shared token store so later test files start clean
.tok$set_sqlite_path(NULL)
.tok$set_passphrase(NULL)
.tok$set_send_mail(NULL)
.tok$set_sql_config_db(NULL)
