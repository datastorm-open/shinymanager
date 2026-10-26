context("test-reset-password-sql")

# SQL backend, with a dedicated DBI / RSQLite configuration (the templates of
# inst/sql_config target real servers: PostgreSQL, SQL Server, ...)
sql_tmp <- tempfile(fileext = ".sqlite")
sql_yml <- tempfile(fileext = ".yml")
writeLines(c(
  "r_packages: [RSQLite]",
  "connect_every_request: true",
  "connect:",
  "  drv: !expr RSQLite::SQLite()",
  paste0("  dbname: \"", gsub("\\\\", "/", sql_tmp), "\""),
  "tables:",
  "  credentials:",
  "    tablename: credentials",
  "    init: CREATE TABLE {`tablename`} (\"user\" varchar(100) PRIMARY KEY, \"password\" varchar(256), \"start\" date, \"expire\" date, \"admin\" boolean, \"email\" varchar(100))",
  "    select: SELECT * FROM {`tablename`} WHERE \"user\" IN ({user*})",
  "    update: UPDATE {`tablename`} SET {`name`} = {value} WHERE \"user\" IN ({udpate_users*})",
  "    delete: DELETE FROM {`tablename`} WHERE \"user\" IN ({del_users*})",
  "  pwd_mngt:",
  "    tablename: pwd_mngt",
  "    init: CREATE TABLE {`tablename`} (\"user\" varchar(100) PRIMARY KEY, \"must_change\" boolean, \"have_changed\" boolean, \"date_change\" date, \"n_wrong_pwd\" smallint, \"temp_pwd_expire\" varchar(19), \"n_user_reset\" integer)",
  "    select: SELECT * FROM {`tablename`} WHERE \"user\" IN ({user*})",
  "    update: UPDATE {`tablename`} SET {`name`} = {value} WHERE \"user\" IN ({udpate_users*})",
  "    delete: DELETE FROM {`tablename`} WHERE \"user\" IN ({del_users*})",
  "  logs:",
  "    tablename: logs",
  "    init: CREATE TABLE {`tablename`} (\"id\" INTEGER PRIMARY KEY, \"user\" varchar(100), \"server_connected\" varchar(30), \"token\" varchar(100), \"logout\" varchar(30), \"status\" varchar(100), \"app\" varchar(100))",
  "    check_token : SELECT * FROM {`tablename`} WHERE \"token\" IN ({token*})",
  "    select: SELECT * FROM {`tablename`} WHERE \"user\" IN ({user*}) AND \"server_connected\" >= {`date_h_begin`}  AND \"server_connected\" <= {`date_h_end`}",
  "    update: UPDATE {`tablename`} SET {`name`} = {value} WHERE \"token\" IN ({token*})"
), sql_yml)

create_sql_db(data.frame(
  user = c("fanny", "victor"),
  password = c("azerty12", "12345A"),
  email = c("fanny@mail.com", NA),
  stringsAsFactors = FALSE
), sql_yml)

sql_checker <- check_credentials(sql_yml)

sql_sent <- new.env()
sql_mail_ok <- function(user, email, temp_password) {
  sql_sent$pwd <- temp_password
  sql_sent$called <- sql_sent$called + 1L
  invisible(TRUE)
}

sql_pwd_mngt <- function(user) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = sql_tmp)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbGetQuery(conn, "SELECT * FROM pwd_mngt WHERE user = ?", params = list(user))
}

test_that("SQL: reset, login with the mailed password, counter and expiration", {
  old <- options(shinymanager.reset_password_validity = 20, shinymanager.reset_password_max = 2)
  .tok$set_send_mail(sql_mail_ok)
  sql_sent$called <- 0L

  res <- reset_pwd_user_email("fanny", "FANNY@mail.com")
  expect_true(res$result)
  expect_equal(sql_sent$called, 1L)
  expect_true(sql_checker("fanny", sql_sent$pwd)$result)
  expect_false(sql_checker("fanny", "azerty12")$result)

  pm <- sql_pwd_mngt("fanny")
  expect_true(as.logical(pm$must_change))
  expect_true(nzchar(pm$temp_pwd_expire))
  expect_equal(pm$n_user_reset, 1)
  expect_false(is_temp_pwd_expired("fanny"))

  expect_true(reset_pwd_user_email("fanny", "fanny@mail.com")$result)
  expect_equal(reset_pwd_user_email("fanny", "fanny@mail.com")$reason, "too_many_resets")

  # a successful login with a non temporary password restarts the counter
  reset_login_counters("fanny")
  expect_equal(sql_pwd_mngt("fanny")$n_user_reset, 0)

  options(old)
})

test_that("SQL: no stored email, unknown user", {
  .tok$set_send_mail(sql_mail_ok)
  old <- options(shinymanager.reset_password = "username")
  expect_equal(reset_pwd_user_email("victor")$reason, "no_stored_email")
  expect_equal(reset_pwd_user_email("nobody")$reason, "unknown_user")
  options(old)
})

test_that("SQL: same answer for known and unknown users without email column", {
  .tok$set_send_mail(sql_mail_ok)
  old <- options(shinymanager.email_column = "mail")
  expect_warning(res_known <- reset_pwd_user_email("fanny", "fanny@mail.com"), "mail")
  expect_warning(res_unknown <- reset_pwd_user_email("nobody", "nobody@mail.com"), "mail")
  expect_equal(res_known$reason, "no_email_column")
  expect_equal(res_unknown$reason, "no_email_column")
  options(old)
})

test_that("SQL: no pwd_mngt row, no email sent and password unchanged", {
  .tok$set_send_mail(sql_mail_ok)
  sql_sent$called <- 0L
  old <- options(shinymanager.reset_password = "username")
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = sql_tmp)
  DBI::dbExecute(conn, "UPDATE credentials SET email = 'victor@mail.com' WHERE user = 'victor'")
  DBI::dbExecute(conn, "DELETE FROM pwd_mngt WHERE user = 'victor'")
  DBI::dbDisconnect(conn)

  expect_warning(res <- reset_pwd_user_email("victor"), "pwd_mngt")
  expect_equal(res$reason, "db_error")
  expect_equal(sql_sent$called, 0L)
  expect_true(sql_checker("victor", "12345A")$result)
  options(old)
})

# reset the shared token store so later test files start clean
.tok$set_sqlite_path(NULL)
.tok$set_passphrase(NULL)
.tok$set_send_mail(NULL)
.tok$set_sql_config_db(NULL)
