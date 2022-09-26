context("test-language")

lan_en <- use_language()

test_that("create language", {
  expect_is(lan_en, "R6")
})

test_that("get language", {
  l_all <- lan_en$get_all()
  expect_is(l_all, "list")
  expect_true(length(l_all) > 0)
  
  for(lab in 1:length(l_all)){
    tmp_label <- names(l_all)[lab]
    expect_equal(lan_en$get(tmp_label), l_all[[lab]])
  }
})

test_that("other language", {
  lan_fr <- use_language()
  lan_fr$set_language("fr")
  
  l_all <- lan_fr$get_all()
  expect_is(l_all, "list")
  expect_true(length(l_all) > 0)
  
  expect_error(lan_fr$set_language("bad"))
})
