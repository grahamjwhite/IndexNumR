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

  expect_equal(kennedyBeta(reg), c(`(Intercept)` = 0.70562777968,
                                   D = -0.2105345407,
                                   prodID2 = -1.0026933120,
                                   prodID3 = -0.6817663689,
                                   prodID4 = -1.3016118124))

})
