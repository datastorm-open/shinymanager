context("test-secure-server")

# server side checks of secure_server(): admin operations and password change
# need a valid token (admin for the admin operations)

sec_tmp <- tempfile(fileext = ".sqlite")
create_db(
  credentials_data = data.frame(
    user = c("alice", "bob"),
    password = c("azerty12", "12345A"),
    admin = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  ),
  sqlite_path = sec_tmp, passphrase = "secret"
)
sec_checker <- check_credentials(sec_tmp, passphrase = "secret")

sec_server <- function(input, output, session) {
  secure_server(check_credentials = sec_checker, timeout = 0, keep_token = TRUE)
}

# the token store only keeps the user info of a valid token
add_sec_token <- function(user, admin) {
  token <- .tok$generate(user)
  .tok$add(token, list(user = user, admin = admin))
  token
}

test_that("token: removing it also removes its user info", {
  token <- add_sec_token("bob", FALSE)
  expect_true(.tok$is_active(token))
  .tok$remove(token)
  expect_false(.tok$is_active(token))
  expect_null(.tok$get(token))
  expect_false(.tok$is_active(NULL))
})

test_that("admin reset is refused without token or with a non admin token", {
  for (token in list(NULL, add_sec_token("bob", FALSE))) {
    local_mocked_bindings(getToken = function(session = NULL) token)
    shiny::testServer(sec_server, {
      session$setInputs(`admin-reset_pwd` = "alice")
      session$setInputs(`admin-reseted_password` = 1)
      session$setInputs(`admin-reseted_password` = 2)
    })
    expect_true(sec_checker("alice", "azerty12")$result)
  }
})

test_that("admin reset works with a valid admin token, not after its removal", {
  token <- add_sec_token("alice", TRUE)
  local_mocked_bindings(getToken = function(session = NULL) token)
  shiny::testServer(sec_server, {
    session$setInputs(`admin-reset_pwd` = "bob")
    session$setInputs(`admin-reseted_password` = 1)
  })
  expect_false(sec_checker("bob", "12345A")$result)

  # revoked while the admin page is open: no more change
  db_before <- read_db_decrypt(sec_tmp, "credentials", "secret")
  shiny::testServer(sec_server, {
    .tok$remove(token)
    session$setInputs(`admin-reset_pwd` = "alice")
    session$setInputs(`admin-reseted_password` = 1)
  })
  expect_identical(read_db_decrypt(sec_tmp, "credentials", "secret")$password, db_before$password)
})

test_that("password change is refused with a revoked token", {
  token <- add_sec_token("bob", FALSE)
  local_mocked_bindings(getToken = function(session = NULL) token)
  shiny::testServer(sec_server, {
    .tok$remove(token)
    session$setInputs(`password-pwd_one` = "Newpwd123", `password-pwd_two` = "Newpwd123")
    session$setInputs(`password-update_pwd` = 1)
  })
  expect_false(sec_checker("bob", "Newpwd123")$result)
})

# reset the shared token store so later test files start clean
.tok$set_sqlite_path(NULL)
.tok$set_passphrase(NULL)
.tok$set_send_mail(NULL)
.tok$set_sql_config_db(NULL)
