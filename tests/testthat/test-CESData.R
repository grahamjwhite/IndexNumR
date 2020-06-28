
test_that("CESData produces the same numbers as CES_sigma_2", {

  expect_equal(CESData(2), CES_sigma_2)

})
