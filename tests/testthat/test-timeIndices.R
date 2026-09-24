context("monthIndex test functions")

load(system.file("testdata","testData_monthIndex.RData",package = "IndexNumR"))

weekMethods <- c("majority","fourWeek","naive","wholeOnly")
dayOfWeek <- c("wed", "fri")

for(i in seq_along(weekMethods)){
  for(j in seq_along(dayOfWeek)){
    test_that("monthIndex function returns the correct values for different week treatments",{
      expect_equal(monthIndex(testData[[paste0("date_",dayOfWeek[j])]],
                              overlapWeeks = weekMethods[i]),
                   (testData[[paste0(weekMethods[i],"_",dayOfWeek[j])]]))
    })
  }
}

test_that("error is thrown when wrong option for overlapWeeks is given",{
  expect_error(monthIndex(testData$date_wed, overlapWeeks = "wrong option"),
               "Not a valid option for parameter overlapWeeks.")
})

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


test_that("monthIndex with majority weeks starts at 1", {

  # 1 September 2017 is a Friday, so its week belongs mostly to August
  dates <- as.Date(c("2017-09-01", "2017-09-15", "2017-10-15"))

  expect_equal(monthIndex(dates, overlapWeeks = "majority"), c(1, 2, 3))
})


test_that("monthIndex with majority weeks handles month and year ends", {

  # 29 December 2025 is a Monday; its week is mostly in January 2026
  # 31 March 2026 is a Tuesday; its week is mostly in April
  dates <- as.Date(c("2025-12-15", "2025-12-29", "2026-01-15", "2026-03-31", "2026-04-15"))

  expect_equal(monthIndex(dates, overlapWeeks = "majority"), c(1, 2, 2, 5, 5))
})
