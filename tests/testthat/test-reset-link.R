context("test-reset-link")

new_link_db <- function(with_email = TRUE) {
  tmp <- tempfile(fileext = ".sqlite")
  creds <- data.frame(user = "fanny", password = "azerty12", stringsAsFactors = FALSE)
  if (with_email) creds$email <- "fanny@mail.com"
  create_db(credentials_data = creds, sqlite_path = tmp, passphrase = "secret")
  tmp
}

clear_link_warnings <- function() {
  rm(list = ls(shinymanager:::reset_link_warnings), envir = shinymanager:::reset_link_warnings)
}

restore_tok <- function() {
  .tok$set_sqlite_path(NULL)
  .tok$set_passphrase(NULL)
  .tok$set_sql_config_db(NULL)
  .tok$set_send_mail(NULL)
}

test_that("the reset link needs a send_mail function", {
  on.exit(restore_tok())
  .tok$set_sqlite_path(new_link_db())
  .tok$set_passphrase("secret")
  .tok$set_send_mail(NULL)
  expect_match(reset_link_unavailable_reason(), "send_mail")
})

test_that("the reset link needs a SQLite / SQL database", {
  on.exit(restore_tok())
  .tok$set_sqlite_path(NULL)
  .tok$set_sql_config_db(NULL)
  .tok$set_send_mail(function(user, email, temp_password) invisible(TRUE))
  expect_match(reset_link_unavailable_reason(), "database")
})

test_that("the reset link needs an email column", {
  on.exit(restore_tok())
  .tok$set_sqlite_path(new_link_db(with_email = FALSE))
  .tok$set_passphrase("secret")
  .tok$set_send_mail(function(user, email, temp_password) invisible(TRUE))
  expect_match(reset_link_unavailable_reason(), "'email' column")

  # custom column name
  old <- options(shinymanager.email_column = "mail")
  on.exit(options(old), add = TRUE)
  .tok$set_sqlite_path(new_link_db(with_email = TRUE))
  expect_match(reset_link_unavailable_reason(), "'mail' column")
})

test_that("the reset link is available with a complete configuration", {
  on.exit(restore_tok())
  .tok$set_sqlite_path(new_link_db())
  .tok$set_passphrase("secret")
  .tok$set_send_mail(function(user, email, temp_password) invisible(TRUE))
  expect_null(reset_link_unavailable_reason())
})

test_that("the hidden link warning is given once per reason", {
  clear_link_warnings()
  on.exit(clear_link_warnings())
  expect_warning(warn_reset_link_hidden("reason A"), "link is hidden: reason A")
  expect_silent(warn_reset_link_hidden("reason A"))
  expect_warning(warn_reset_link_hidden("reason B"), "reason B")
})

test_that("auth module: the link is hidden with a warning when send_mail is missing", {
  clear_link_warnings()
  on.exit({
    clear_link_warnings()
    restore_tok()
  })
  .tok$set_send_mail(NULL)
  checker <- check_credentials(new_link_db(), passphrase = "secret")
  srv <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      auth_server(input, output, session, check_credentials = checker)
    })
  }
  old <- options(shinymanager.reset_password = TRUE)
  on.exit(options(old), add = TRUE)
  expect_warning(
    shiny::testServer(srv, args = list(id = "auth"), {}),
    "send_mail"
  )
  # option not set: no check, no warning
  options(shinymanager.reset_password = FALSE)
  expect_silent(shiny::testServer(srv, args = list(id = "auth"), {}))
})

test_that("every language has the same labels as english", {
  en <- names(use_language("en")$get_all())
  for (lan in shinymanager:::v_language_registered) {
    labels <- names(use_language(lan)$get_all())
    expect_setequal(labels, en)
    expect_false(any(duplicated(labels)), info = lan)
  }
})
