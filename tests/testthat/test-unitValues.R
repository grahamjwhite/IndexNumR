context("unitValues functions")

# test vectors of dates
dat_year <- as.Date(c("2017-01-01","2017-02-01","2018-01-01","2018-04-02","2019-01-01"))
dat_quarter <- as.Date(c("2017-01-01","2017-04-01","2017-08-02","2017-09-01"))
dat_month <- as.Date(c("2017-01-01","2017-02-01","2017-02-02","2017-03-01"))

test_that("time index functions return the correct index values",{
  expect_equal(yearIndex(dat_year),c(1,1,2,2,3))
  expect_equal(quarterIndex(dat_quarter),c(1,2,3,3))
  expect_equal(monthIndex(dat_month),c(1,2,2,3))
})
