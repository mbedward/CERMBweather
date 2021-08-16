test_that("ymd to date checks for equal vector lengths", {
  expect_error(bom_ymd_to_date(rep(2020, 31), 4, 1:31), regexp = "same length")
})


test_that("ymd to date with vectors", {
  expect_equal(bom_ymd_to_date(2020, 4, 23), as.Date("2020-04-23"))

  years <- c(rep(1999, 31), rep(2000, 61))
  months <- c(rep(12, 31), rep(1, 31), rep(2, 29), 3)
  days <- c(1:31, 1:31, 1:29, 1)

  expected <- seq(as.Date("1999-12-01"), as.Date("2000-03-01"), by = "1 day")
  expect_equal(bom_ymd_to_date(years, months, days), expected)
})


test_that("ymd to date with data frame and matrix", {
  dat <- data.frame(year = c(rep(1999, 31), rep(2000, 61)),
                    month = c(rep(12, 31), rep(1, 31), rep(2, 29), 3),
                    day = c(1:31, 1:31, 1:29, 1))

  expected <- seq(as.Date("1999-12-01"), as.Date("2000-03-01"), by = "1 day")

  expect_equal(bom_ymd_to_date(dat), expected)

  expect_equal(bom_ymd_to_date(as.matrix(dat)), expected)
})


test_that("date to ymd", {
  dates <- as.Date("2020-04-23")
  expected <- data.frame(year = 2020, month = 4, day = 23)
  expect_equal(expected, bom_date_to_ymd(dates))

  dates <- seq(as.Date("1999-12-01"), as.Date("2000-03-01"), by = "1 day")

  expected <- data.frame(year = c(rep(1999, 31), rep(2000, 61)),
                         month = c(rep(12, 31), rep(1, 31), rep(2, 29), 3),
                         day = c(1:31, 1:31, 1:29, 1) )

  expect_equal(expected, bom_date_to_ymd(dates))
})
