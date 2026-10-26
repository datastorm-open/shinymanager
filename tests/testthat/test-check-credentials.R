context("test-check-credentials")

test_that("check_credentials works", {

  credentials <- data.frame(
    user = c("fanny", "victor"),
    password = c("azerty", "12345"),
    stringsAsFactors = FALSE
  )

  expect_is(check_credentials(credentials), "function")
  expect_is(check_credentials(credentials)("fanny", "azerty"), "list")
  expect_true(check_credentials(credentials)("fanny", "azerty")$result)
  expect_false(check_credentials(credentials)("fanny", "12345")$result)
})


test_that("check_credentials (expired) works", {

  credentials <- data.frame(
    user = c("fanny", "victor", "benoit"),
    password = c("azerty", "12345", "azerty"),
    comment = c("alsace", "auvergne", "bretagne"),
    expire = Sys.Date() + c(10, -10, 10),
    stringsAsFactors = FALSE
  )

  expect_true(check_credentials(credentials)("fanny", "azerty")$result)
  expect_false(check_credentials(credentials)("fanny", "azerty")$expired)
  expect_false(check_credentials(credentials)("victor", "12345")$result)
  expect_true(check_credentials(credentials)("victor", "12345")$expired)
})


test_that("check_credentials (applications) works", {

  credentials <- data.frame(
    user = c("fanny", "victor", "benoit"),
    password = c("azerty", "12345", "azerty"),
    comment = c("alsace", "auvergne", "bretagne"),
    applications = c("app1;app2", "app1", "app2"),
    stringsAsFactors = FALSE
  )

  old <- options("shinymanager.application" = "app2")
  on.exit(options(old), add = TRUE)

  expect_true(check_credentials(credentials)("fanny", "azerty")$result)
  expect_true(check_credentials(credentials)("fanny", "azerty")$authorized)
  expect_false(check_credentials(credentials)("victor", "12345")$result)
  expect_false(check_credentials(credentials)("victor", "12345")$authorized)
})


test_that("check_credentials gives a clear error for a missing file", {
  expect_error(check_credentials("does_not_exist.sqlite"), "File not found: 'does_not_exist.sqlite'")
  expect_error(check_credentials("does_not_exist.yml"), "working directory")
  expect_error(check_credentials("not_a_db.txt"), "must be a data.frame")
})
