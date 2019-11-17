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
