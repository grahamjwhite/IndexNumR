
test_that("Elasticity function returns 2 for the CES_sigma_2 dataset", {

  elas <- elasticity(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID",
                     compIndex = "ces")

  # floating point errors lead to this failing without a tolerance
  expect_equal(elas$sigma, 2, tolerance = 0.0000001)

})


test_that("elasticity does not depend on the row order of the data", {

  set.seed(1)
  shuffled <- CES_sigma_2[sample(nrow(CES_sigma_2)),]

  elas <- elasticity(shuffled, pvar = "prices", qvar = "quantities", pervar = "time",
                     prodID = "prodID", compIndex = "ces")

  expect_equal(elas$sigma, 2, tolerance = 0.0000001)
})


test_that("elasticity returns NA with a warning when there is no solution", {

  expect_warning(elas <- elasticity(CES_sigma_2, pvar = "prices", qvar = "quantities",
                                    pervar = "time", prodID = "prodID", lower = 5, upper = 10),
                 "No solution for sigma was found")

  expect_true(all(is.na(elas$allsigma)))
})
