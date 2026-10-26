context("test-icons")

icon_class <- function(x) x$attribs$class

test_that("sm_icon uses Font Awesome 6 names with a recent fontawesome", {
  old <- shinymanager:::sm_icon_env$fa5
  on.exit(assign("fa5", old, envir = shinymanager:::sm_icon_env))
  assign("fa5", FALSE, envir = shinymanager:::sm_icon_env)
  expect_match(icon_class(sm_icon("pen-to-square")), "fa-pen-to-square")
  expect_match(icon_class(sm_icon("key")), "fa-key")
})

test_that("sm_icon uses Font Awesome 5 names with an old fontawesome / shiny", {
  old <- shinymanager:::sm_icon_env$fa5
  on.exit(assign("fa5", old, envir = shinymanager:::sm_icon_env))
  assign("fa5", TRUE, envir = shinymanager:::sm_icon_env)
  # FA5 names are still known (as aliases) by a recent fontawesome
  expect_match(icon_class(suppressWarnings(sm_icon("pen-to-square"))), "fa-(edit|pen-to-square)")
  expect_identical(
    icon_class(suppressWarnings(sm_icon("xmark"))),
    icon_class(suppressWarnings(shiny::icon("times")))
  )
  # names unchanged between FA5 and FA6
  expect_match(icon_class(sm_icon("key")), "fa-key")
})

test_that("every Font Awesome 6 name has a Font Awesome 5 name", {
  expect_true(all(nzchar(shinymanager:::fa5_names)))
  expect_false(any(duplicated(names(shinymanager:::fa5_names))))
})
