context("unitValues functions")

# a test vector of dates
dat_year <- as.Date(c("2017-01-01","2017-02-01","2018-01-01","2018-04-02","2019-01-01"))

test_that("yearIndex returns the correct index values",{
  expect_equal(yearIndex(dat_year),c(1,1,2,2,3))
})
