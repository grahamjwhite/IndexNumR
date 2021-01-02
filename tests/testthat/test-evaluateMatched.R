load(system.file("testdata", "testData_evaluateMatched.RData", package = "IndexNumR"))

test_that("evaluate matched returns the right results when a product disappears", {

  result_pop <- evaluateMatched(CES_sigma_2[-15,], "prices", "quantities", "time", "prodID")
  result_fixedbase <- evaluateMatched(CES_sigma_2[-15,], "prices", "quantities", "time", "prodID", output = "fixedbase")

  expect_equal(e_pop, result_pop)
  expect_equal(e_fixedbase, result_fixedbase)

})

rm(e_pop, e_fixedbase)
