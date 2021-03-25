context("test-modules-ui")

test_that("auth UI works", {

  auth_ui_tags <- auth_ui(id = "auth")

  expect_is(auth_ui_tags, "shiny.tag.list")
})


test_that("pwd UI works", {

  pwd_ui_tags <- pwd_ui(id = "pwd")

  expect_is(pwd_ui_tags, "shiny.tag.list")
})


test_that("admin UI works", {

  admin_ui_tags <- admin_ui(id = "admin")

  expect_is(admin_ui_tags, "shiny.tag.list")
})


test_that("edit_user UI works", {

  credentials <- data.frame(
    user = c("fanny", "victor", "benoit"),
    password = c("azerty", "12345", "azerty"),
    comment = c("alsace", "auvergne", "bretagne"),
    expire = Sys.Date() + c(10, -10, 10),
    stringsAsFactors = FALSE
  )

  edit_user_ui_tags <- edit_user_ui(id = "edit", credentials = credentials)

  edit_user_ui_tags <- edit_user_ui(id = "edit", 
                                    credentials = credentials, 
                                    username = "fanny")
  
  expect_is(edit_user_ui_tags, "shiny.tag.list")
})


test_that("logs UI works", {

  logs_ui_tags <- logs_ui(id = "logs")

  expect_is(logs_ui_tags, "shiny.tag.list")
})


test_that("fab_button works", {
  
  fab_button_tags <- fab_button()
  
  expect_is(fab_button_tags, "shiny.tag.list")
})

test_that("secure_app works", {
  
  sa <- secure_app(fluidPage())
  
  expect_is(sa, "function")
})
