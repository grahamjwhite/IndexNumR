context("value tests")

test_that("values are calculated correctly when no matching required", {

  result <- matrix(c(10, 13, 11, 12, 15, 13, 14, 17, 15, 18, 16, 17), nrow = 12, ncol = 1)

  expect_equal(values(CES_sigma_2, pvar = "prices", qvar = "quantities",
                      pervar = "time", prodID = "prodID", sample = "unmatched"),
               result)

})

# Set up some test data
# boolean vector to drop data on product 2 in time period 3
exclude <- CES_sigma_2$time == 3 & CES_sigma_2$prodID == 2

# get the data dropping as above
testData <- CES_sigma_2[!exclude,]


test_that("Values are calculated correctly when following period matching", {

  # calculate values
  result <- values(testData, "prices", "quantities", "time", "prodID", sample = "matched", matchPeriod = "following")

  expect_equal(result, as.matrix(c(10, 9.418941819, 9.26029654, 12, 15, 13, 14, 17, 15, 18, 16, NA)))

})

test_that("Values are calculated correctly when previous period matching", {

  # calculate values
  result <- values(testData, "prices", "quantities", "time", "prodID", sample = "matched", matchPeriod = "previous")

  expect_equal(result, as.matrix(c(NA, 13, 9.26029654, 10.12852842, 15, 13, 14, 17, 15, 18, 16, 17)))

})


test_that("Error is thrown when wrong matchPeriod is given",{

  expect_error(values(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID",
                      matchPeriod = "wrong argument"),
               "Not a valid matchPeriod argument")

})
