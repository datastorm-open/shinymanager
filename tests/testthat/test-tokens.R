context("test-tokens")

.tok <- .tokens$new()

token_admin = "token_admin"
list_info_admin <- list(user = "benoit", role = "full", admin = TRUE)

token_not_admin = "token_not_admin"
list_info_not_admin <- list(user = "victor", role = "full", admin = FALSE)

test_that("create token", {
  expect_silent(.tok$add(token_admin, list_info_admin))
  expect_equal(.tok$get(token_admin), list_info_admin)
  
  expect_silent(.tok$add(token_not_admin, list_info_not_admin))
  expect_equal(.tok$get(token_not_admin), list_info_not_admin)
})

test_that("valid token", {
  # is valid TRUE one time only
  expect_true(.tok$is_valid(token_admin))
  expect_true(.tok$is_valid(token_not_admin))
  expect_false(.tok$is_valid("bad_token"))
  
  expect_false(.tok$is_valid(token_admin))
  expect_false(.tok$is_valid(token_not_admin))
  
  expect_true(.tok$is_valid_server(token_admin))
  expect_true(.tok$is_valid_server(token_not_admin))
  expect_false(.tok$is_valid_server("bad_token"))
})

test_that("get token user", {
  expect_equal(.tok$get_user(token_admin), "benoit")
  expect_equal(.tok$get_user(token_not_admin), "victor")
  expect_null(.tok$get_user("bad_token"))
})

test_that("get token admin", {
  expect_true(.tok$is_admin(token_admin))
  expect_false(.tok$is_admin(token_not_admin))
  expect_false(.tok$is_admin("bad_token"))
})

test_that("token passphrase", {
  expect_null(.tok$get_passphrase())
  expect_silent(.tok$set_passphrase("secret"))
  expect_equal(.tok$get_passphrase(), "secret")
})

test_that("token sqlite", {
  expect_null(.tok$get_sqlite_path())
  expect_silent(.tok$set_sqlite_path("path.sqlite"))
  expect_equal(.tok$get_sqlite_path(), "path.sqlite")
})

test_that("token timeout", {
  expect_silent(.tok$set_timeout(10))
  expect_equal(.tok$get_timeout(), 10)
  
  expect_silent(.tok$set_timeout(0))
  expect_equal(.tok$get_timeout(), 0)
})

test_that("token timeout valid", {
  expect_true(.tok$is_valid_timeout(token_admin))
  expect_true(.tok$is_valid_timeout(token_not_admin))
})

test_that("token sha256", {
  sh <- .tok$generate("benoit")
  expect_is(sh, "sha256")
})

test_that("token remove", {
  expect_true(.tok$is_valid_server(token_not_admin))
  expect_silent(.tok$remove(token_not_admin))
  expect_false(.tok$is_valid_server(token_not_admin))
})
