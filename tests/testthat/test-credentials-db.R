context("test-credentials-db")

tmp_sqlite <- tempfile(fileext = ".sqlite")
credentials <- data.frame(
  user = c("fanny", "victor"),
  password = c("azerty", "12345"),
  stringsAsFactors = FALSE
)

test_that("create_db works", {
  expect_silent(create_db(credentials_data = credentials, sqlite_path = tmp_sqlite, passphrase = "secret"))
  expect_true(file.size(tmp_sqlite) > 0)
})


test_that("read_db_decrypt works", {

  db_read <- read_db_decrypt(conn = tmp_sqlite, name = "credentials", passphrase = "secret")

  expect_is(db_read, "data.frame")
  expect_error(read_db_decrypt(conn = tmp_sqlite, name = "credentials", passphrase = "wrong"))
})
