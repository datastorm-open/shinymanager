context("test-rbind-fill")

test_that("rbind_fill fills the columns missing in the new rows with NA", {
  df <- data.frame(user = "a", n_wrong_pwd = 0, custom = "x", stringsAsFactors = FALSE)
  new <- data.frame(user = "b", n_wrong_pwd = 1, stringsAsFactors = FALSE)
  res <- rbind_fill(df, new)
  expect_equal(colnames(res), c("user", "n_wrong_pwd", "custom"))
  expect_equal(res$user, c("a", "b"))
  expect_equal(res$custom, c("x", NA))
})

test_that("rbind_fill adds the columns missing in the table, keeping its order", {
  df <- data.frame(user = "a", must_change = "FALSE", stringsAsFactors = FALSE)
  new <- data.frame(must_change = "TRUE", n_user_reset = 0, user = "b", stringsAsFactors = FALSE)
  res <- rbind_fill(df, new)
  expect_equal(colnames(res), c("user", "must_change", "n_user_reset"))
  expect_equal(res$must_change, c("FALSE", "TRUE"))
  expect_equal(res$n_user_reset, c(NA, 0))
})

test_that("rbind_fill works on an empty table", {
  df <- data.frame(user = character(0), custom = character(0), stringsAsFactors = FALSE)
  new <- data.frame(user = "b", status = "Success", stringsAsFactors = FALSE)
  res <- rbind_fill(df, new)
  expect_equal(nrow(res), 1)
  expect_equal(colnames(res), c("user", "custom", "status"))
  expect_true(is.na(res$custom))
})

test_that("logs are written when the SQLite logs table has a custom column", {
  tmp_sqlite <- tempfile(fileext = ".sqlite")
  create_db(
    credentials_data = data.frame(user = "fanny", password = "azerty12", stringsAsFactors = FALSE),
    sqlite_path = tmp_sqlite, passphrase = "secret"
  )
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = tmp_sqlite)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  logs <- read_db_decrypt(conn, name = "logs", passphrase = "secret")
  logs$custom <- character(0)
  write_db_encrypt(conn, value = logs, name = "logs", passphrase = "secret")

  .tok$set_sqlite_path(tmp_sqlite)
  .tok$set_passphrase("secret")
  .tok$set_sql_config_db(NULL)
  on.exit({
    .tok$set_sqlite_path(NULL)
    .tok$set_passphrase(NULL)
  }, add = TRUE)

  save_logs_failed("fanny", status = "Wrong pwd")
  logs <- read_db_decrypt(conn, name = "logs", passphrase = "secret")
  expect_equal(nrow(logs), 1)
  expect_equal(logs$status, "Wrong pwd")
  expect_true("custom" %in% colnames(logs))
})
