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

  res <- reset_pwd_user_email("victor", "victor@mail.com")

  expect_false(res$result)
  db_after <- read_db_decrypt(tmp_sqlite, "credentials", "secret")
  expect_identical(db_after$password[db_after$user == "victor"], pwd_before)
})

test_that("no reset when send_mail is not configured", {
  .tok$set_send_mail(NULL)
  expect_false(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
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

  expect_false(reset_pwd_user_email("alice", "alice@mail.com")$result)
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
  expect_equal(reset_pwd_user_email("victor", "victor@mail.com")$reason, "mail_failed")
})

test_that("reason is no_mailer when send_mail is not configured", {
  .tok$set_send_mail(NULL)
  expect_equal(reset_pwd_user_email("fanny", "fanny@mail.com")$reason, "no_mailer")
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

test_that("save_reset_logs writes a mapped status, skips config/empty reasons", {
  .tok$set_sqlite_path(tmp_sqlite)

  before <- read_db_decrypt(tmp_sqlite, "logs", "secret")
  save_reset_logs("fanny", "success")
  after <- read_db_decrypt(tmp_sqlite, "logs", "secret")
  expect_equal(nrow(after), nrow(before) + 1)
  expect_equal(after$status[nrow(after)], "Reset password")

  save_reset_logs("someone", "unknown_user")
  logs2 <- read_db_decrypt(tmp_sqlite, "logs", "secret")
  expect_equal(logs2$status[nrow(logs2)], "Reset password: unknown user")

  # config / empty-input reasons must not create a log line
  n <- nrow(logs2)
  save_reset_logs("fanny", "empty_input")
  save_reset_logs("fanny", "no_mailer")
  expect_equal(nrow(read_db_decrypt(tmp_sqlite, "logs", "secret")), n)
})

# reset the shared token store so later test files start clean
.tok$set_sqlite_path(NULL)
.tok$set_passphrase(NULL)
.tok$set_send_mail(NULL)
.tok$set_sql_config_db(NULL)
