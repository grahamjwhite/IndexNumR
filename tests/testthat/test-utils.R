context("utilities")

continuous1 <- c(1,2,3,4,5,6)
continuous2 <- c(1,1,1,2,2,2,3,3,4,5,5,5)
gaps1 <- c(1,2,4,5,6)
gaps2 <- c(1,1,1,2,2,4,5,5,5)
gaps3 <- c(1,1,1,2,2,2,5,5,5)

test_that("gaps in a vector are detected",{
  expect_equal(isContinuous(continuous1),list(result=TRUE))
  expect_equal(isContinuous(continuous2),list(result=TRUE))
  expect_equal(isContinuous(gaps1),list(result=FALSE,missing=3))
  expect_equal(isContinuous(gaps2),list(result=FALSE,missing=3))
  expect_equal(isContinuous(gaps3),list(result=FALSE,
                                        missing=as.integer(c(3,4))))
})


test_that("checkTypes converts factor columns to numeric", {

  testData <- CES_sigma_2
  testData$time <- as.factor(testData$time)

  testData <- checkTypes(testData, pervar = "time", pvar = "prices", qvar = "quantities")

  expect_equal(inherits(testData$time, "numeric"), TRUE)

})


test_that("KennedyBeta calculates the correct adjustment", {

  dat <- CES_sigma_2[CES_sigma_2$time <= 2,]
  dat$prodID <- as.factor(dat$prodID)
  dat$D <- ifelse(dat$time == 2, 1, 0)

  reg <- lm(prices ~ D + prodID, data = dat)

  expect_equal(kennedyBeta(reg), c(`(Intercept)` = 1.95953776,
                                   D = -0.194869792,
                                   prodID2 = -1.139739583,
                                   prodID3 = -0.914739583,
                                   prodID4 = -1.364739583))

})


test_that("checkTypes can detect bad variable types", {

  bad <- letters[1:5]
  good <- 1:5

  df <- data.frame(p = bad, q = good, period = good, prodID = LETTERS[1:5])

  expect_error(checkTypes(df, "p", "q", "period"),
               "Please correct input data types. Price variable is not numeric and cannot be coerced to numeric")

  df <- data.frame(p = good, q = bad, period = good, prodID = LETTERS[1:5])

  expect_error(checkTypes(df, "p", "q", "period"),
               "Please correct input data types. Quantity variable is not numeric and cannot be coerced to numeric")

  df <- data.frame(p = good, q = good, period = bad, prodID = LETTERS[1:5])

  expect_error(checkTypes(df, "p", "q", "period"),
               "Please correct input data types. Time period variable is not numeric and cannot be coerced to numeric")


})


test_that("checkTypes converts factors using their labels, not their level codes", {

  df <- data.frame(p = factor(c("1.5", "0.5", "10")),
                   q = factor(c(2, 3, 1)),
                   period = factor(c("1", "10", "2")),
                   prodID = 1:3)

  checked <- checkTypes(df, "p", "q", "period", "prodID")

  expect_equal(checked$p, c(1.5, 0.5, 10))
  expect_equal(checked$q, c(2, 3, 1))
  expect_equal(checked$period, c(1, 10, 2))

})


test_that("checkTypes converts character columns and keeps missing values", {

  df <- data.frame(p = c("1.5", NA, "2"), q = c("1", "2", NA),
                   period = c("1", "2", "3"), prodID = c("a", "b", "c"))

  checked <- checkTypes(df, "p", "q", "period", "prodID")

  expect_equal(checked$p, c(1.5, NA, 2))
  expect_equal(checked$q, c(1, 2, NA))
  expect_equal(checked$period, c(1, 2, 3))
  expect_identical(checked$prodID, c("a", "b", "c"))

})


test_that("checkTypes names the values that cannot be converted", {

  df <- data.frame(p = c("$1.60", "1.5"), q = 1:2, period = 1:2, prodID = 1:2)

  expect_error(checkTypes(df, "p", "q", "period", "prodID"), "'\\$1.60'")

})


test_that("checkTypes rejects dates and fractional time periods", {

  df <- data.frame(p = 1:2, q = 1:2, period = as.Date(c("2020-01-01", "2020-02-01")),
                   prodID = 1:2)
  expect_error(checkTypes(df, "p", "q", "period", "prodID"), "monthIndex")

  df$period <- c(1, 1.5)
  expect_error(checkTypes(df, "p", "q", "period", "prodID"), "whole numbers")

})


test_that("checkTypes drops unused levels of a factor product identifier", {

  df <- data.frame(p = 1:2, q = 1:2, period = 1:2,
                   prodID = factor(c("a", "b"), levels = c("a", "b", "c")))

  checked <- checkTypes(df, "p", "q", "period", "prodID")

  expect_identical(checked$prodID, factor(c("a", "b")))

})


test_that("checkTypes skips variables given as an empty string", {

  df <- data.frame(period = 1:2, prodID = c("a", "b"))

  expect_equal(checkTypes(df, "", "", "period", "prodID"), df)

})


test_that("validateInput checks the input is a data frame and the column names", {

  expect_error(validateInput(as.matrix(CES_sigma_2), "prices", "quantities", "time", "prodID"),
               "must be a data frame")
  expect_error(validateInput(CES_sigma_2, "price", "quantities", "time", "prodID"),
               "are not column names")
  expect_equal(validateInput(CES_sigma_2, "prices", "quantities", "time", "prodID"),
               CES_sigma_2)

})


test_that("validateInput converts a data.table without modifying it", {

  skip_if_not_installed("data.table")

  dt <- data.table::as.data.table(CES_sigma_2)
  checked <- validateInput(dt, "prices", "quantities", "time", "prodID")

  expect_identical(class(checked), "data.frame")
  expect_equal(checked, CES_sigma_2, check.attributes = FALSE)
  expect_true(data.table::is.data.table(dt))

})


test_that("windowMatch removes products correctly", {

  # remove product 1 from last few periods and product 3 from first few periods
  df <- CES_sigma_2[-c(8:12, 25:26),]

  # window matching should only return products 2 and 4
  wmdf <- windowMatch(df, "time", "prodID")

  expect_equal(wmdf, CES_sigma_2[CES_sigma_2$prodID %in% c(2, 4),])

})


test_that("checkTimeProductUnique finds duplicated products", {

  duped <- CES_sigma_2
  # set the time for the second observation on product 1 to period 1
  # which creates a duplicate time/product combination
  duped$time[2] <- 1

  expect_false(checkTimeProdUnique(duped, "time", "prodID")$result)

})


test_that("fillMissing leaves complete data unchanged", {

  expect_equal(fillMissing(CES_sigma_2, "prices", "quantities", "time", "prodID", 0, 0),
               CES_sigma_2)
})


test_that("checkArguments suggests the right argument name", {

  expect_silent(checkArguments(list(), c("measure"), "priceIndex"))
  expect_silent(checkArguments(list(measure = "mix"), c("measure"), "priceIndex"))
  expect_error(checkArguments(list(prodId = "x"), c("measure"), "priceIndex"),
               "did you mean prodID")
  expect_error(checkArguments(list("x"), c("measure"), "priceIndex"),
               "must be named")
})


test_that("logMean handles equal values", {

  expect_equal(logMean(c(1, 2, 0.5), c(1, 2, 0.25)), c(1, 2, 0.25/log(2)))
})
