
test_that("Elasticity function returns 2 for the CES_sigma_2 dataset", {

  elas <- elasticity(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID",
                     compIndex = "ces")

  # floating point errors lead to this failing without a tolerance
  expect_equal(elas$sigma, 2, tolerance = 0.0000001)

})
