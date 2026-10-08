context("test-logs")

test_that("security_category groups the failed statuses", {
  status <- c("Success", "Wrong pwd", "Unknown user", "Locked Account", "Account locked",
              "Expired", "Unauthorized", "Reset password: expired", "Reset password",
              "Reset password: wrong email", "Reset password: mail failed", NA)
  expect_equal(
    security_category(status),
    c(NA, "Wrong passwords", "Unknown users", "Locked accounts", "Locked accounts",
      "Expired / unauthorized", "Expired / unauthorized", "Expired / unauthorized",
      "Password resets", "Failed password resets", "Failed password resets", NA)
  )
})

test_that("security_events_per_day fills every day of the range", {
  logs <- data.frame(
    server_connected = c("2026-01-01 10:00:00", "2026-01-01 11:00:00", "2026-01-03 09:00:00"),
    category = c("Wrong passwords", "Unknown users", "Wrong passwords"),
    stringsAsFactors = FALSE
  )
  res <- security_events_per_day(logs)
  expect_equal(res$day, c("2026-01-01", "2026-01-02", "2026-01-03"))
  expect_equal(colnames(res), c("day", "Wrong passwords", "Unknown users"))
  expect_equal(res[["Wrong passwords"]], c(1, 0, 1))
  expect_equal(res[["Unknown users"]], c(1, 0, 0))
})

test_that("users_overview gives last connection, counts and locked status", {
  logs <- data.frame(
    user = c("fanny", "fanny", "victor"),
    server_connected = c("2026-01-01 10:00:00", "2026-01-05 10:00:00", "2026-01-02 10:00:00"),
    app = c("app1", "app2", "app1"),
    status = "Success",
    stringsAsFactors = FALSE
  )
  logs_period <- rbind(logs, data.frame(
    user = c("fanny", "victor", "victor"),
    server_connected = "2026-01-06 10:00:00",
    app = "app1",
    status = c("Wrong pwd", "Wrong pwd", "Reset password"),
    stringsAsFactors = FALSE
  ))
  pwd_mngt <- data.frame(user = c("fanny", "victor"), n_wrong_pwd = c(0, 3))

  res <- users_overview(c("fanny", "victor", "nobody"), logs, logs_period, pwd_mngt, pwd_failure_limit = 3)
  expect_equal(res$last_connection, c("2026-01-05 10:00:00", "2026-01-02 10:00:00", NA))
  expect_equal(res$last_app, c("app2", "app1", NA))
  expect_equal(res$n_success, c(2L, 1L, 0L))
  expect_equal(res$n_wrong_pwd, c(1L, 1L, 0L))
  expect_equal(res$n_reset, c(0L, 1L, 0L))
  expect_equal(res$locked, c(FALSE, TRUE, FALSE))

  # no failure limit: no locked column
  res <- users_overview("fanny", logs, logs_period, pwd_mngt)
  expect_false("locked" %in% colnames(res))
})
