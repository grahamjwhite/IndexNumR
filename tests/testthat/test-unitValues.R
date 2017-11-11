context("unitValues functions")

# test vectors of dates
dat_year <- as.Date(c("2017-01-01","2017-02-01","2018-01-01","2018-04-02","2019-01-01"))
dat_quarter <- as.Date(c("2017-01-01","2017-04-01","2017-08-02","2017-09-01"))
dat_month <- as.Date(c("2017-01-01","2017-02-01","2017-02-02","2017-03-01"))
dat_week <- as.Date(c("2016-12-19","2016-12-26","2017-01-02","2017-01-09","2016-12-20"))

test_that("time index functions return the correct index values",{
  expect_equal(yearIndex(dat_year),c(1,1,2,2,3))
  expect_equal(quarterIndex(dat_quarter),c(1,2,3,3))
  expect_equal(monthIndex(dat_month),c(1,2,2,3))
  expect_equal(weekIndex(dat_week),c(1,2,3,4,1))
})

test_that("correct indices are returned if dates are reversed",{
  expect_equal(yearIndex(rev(dat_year)),rev(c(1,1,2,2,3)))
  expect_equal(quarterIndex(rev(dat_quarter)),rev(c(1,2,3,3)))
  expect_equal(monthIndex(rev(dat_month)),rev(c(1,2,2,3)))
  expect_equal(weekIndex(rev(dat_week)),rev(c(1,2,3,4,1)))
})
