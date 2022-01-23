load(system.file("testdata", "testData_evaluateMatched.RData", package = "IndexNumR"))

test_that("evaluate matched returns the right results when a product disappears", {

  result_pop <- evaluateMatched(CES_sigma_2[-15,], "prices", "quantities", "time", "prodID")
  result_fixedbase <- evaluateMatched(CES_sigma_2[-15,], "prices", "quantities", "time", "prodID", output = "fixedbase")

  expect_equal(e_pop, result_pop)
  expect_equal(e_fixedbase, result_fixedbase)

})

rm(e_pop, e_fixedbase)


test_that("productChanges correctly identifies appearing/disappearing products", {

  df <- CES_sigma_2[-c(3,4,15),]

  expect_equal(productChanges(df, "time", "prodID"),
               list(`3` = list(disappearing = c(1, 2)),
                    `4` = list(appearing = 2),
                    `5` = list(appearing = 1)))

})

