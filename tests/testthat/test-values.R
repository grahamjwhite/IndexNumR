context("value tests")

test_that("values are calculated correctly when no matching required", {

  result <- matrix(c(10, 13, 11, 12, 15, 13, 14, 17, 15, 18, 16, 17), nrow = 12, ncol = 1)

  expect_equal(values(CES_sigma_2, pvar = "prices", qvar = "quantities",
                      pervar = "time", prodID = "prodID", sample = "unmatched"),
               result)

})
