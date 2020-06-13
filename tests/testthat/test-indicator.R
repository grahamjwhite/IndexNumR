context("indicator tests")

test_that("price indicator returns correct values for laspeyres method", {

  results <- as.matrix(c(NA, -0.326923077, 4.344176771, 0.406142949,
               -0.806657972,5.845138200, -0.611482951,
               -1.699274598, 4.520355376, -1.027465234,
               4.922147059, 0.139606945))

  expect_equal(priceIndicator(CES_sigma_2,
                 pvar = "prices",
                 qvar = "quantities",
                 prodID = "prodID",
                 pervar = "time",
                 method = "laspeyres"),
               results)

})


test_that("price indicator returns correct values for paasche method", {

  results <- as.matrix(c(NA, -3.234511666, 1.198895655, 0.338354799,
               -5.655012326, 1.890617438, -0.724047979,
               -4.766835358, 1.388564533, -4.499852941,
               1.650519351, 0.025035018))

  expect_equal(priceIndicator(CES_sigma_2,
                              pvar = "prices",
                              qvar = "quantities",
                              prodID = "prodID",
                              pervar = "time",
                              method = "paasche"),
               results)

})


test_that("price indicator returns correct values for bennet method", {

  results <- as.matrix(c(NA, -1.780717371, 2.771536213, 0.372248874,
               -3.230835149, 3.867877819, -0.667765465,
               -3.233054978, 2.954459954, -2.763659088,
               3.286333205, 0.082320982))

  expect_equal(priceIndicator(CES_sigma_2,
                              pvar = "prices",
                              qvar = "quantities",
                              prodID = "prodID",
                              pervar = "time",
                              method = "bennet"),
               results)

})


test_that("price indicator returns correct values for montgomery method", {

  results <- as.matrix(c(NA, -1.278748023, 2.237641633, 0.373294607,
               -2.351385991, 3.239124508, -0.665710587,
               -2.745352531, 2.451685595, -2.287617905,
               2.854834029, 0.083912950))

  expect_equal(priceIndicator(CES_sigma_2,
                              pvar = "prices",
                              qvar = "quantities",
                              prodID = "prodID",
                              pervar = "time",
                              method = "montgomery"),
               results)

})


test_that("quantity indicator returns correct values for laspeyres method", {

  results <- as.matrix(c(NA, 6.234511666, -3.198895655, 0.661645201,
                         8.655012326, -3.890617438, 1.724047979,
                         7.766835358, -3.388564533, 7.499852941,
                         -3.650519351, 0.974964982))

  expect_equal(quantityIndicator(CES_sigma_2,
                              pvar = "prices",
                              qvar = "quantities",
                              prodID = "prodID",
                              pervar = "time",
                              method = "laspeyres"),
               results)

})


test_that("quantity indicator returns correct values for paasche method", {

  results <- as.matrix(c(NA,3.326923077,-6.344176771,0.593857051,
                         3.806657972,-7.845138200,1.611482951,
                         4.699274598,-6.520355376,4.027465234,
                         -6.922147059,0.860393055))

  expect_equal(quantityIndicator(CES_sigma_2,
                               pvar = "prices",
                               qvar = "quantities",
                               prodID = "prodID",
                               pervar = "time",
                               method = "paasche"),
               results)

})


test_that("quantity indicator returns correct values for bennet method", {

  results <- as.matrix(c(NA,4.780717371,-4.771536213,
                         0.627751126,6.230835149,-5.867877819,
                         1.667765465,6.233054978,-4.954459954,
                         5.763659088,-5.286333205,0.917679018))

  expect_equal(quantityIndicator(CES_sigma_2,
                               pvar = "prices",
                               qvar = "quantities",
                               prodID = "prodID",
                               pervar = "time",
                               method = "bennet"),
               results)

})


test_that("quantity indicator returns correct values for montgomery method", {

  results <- as.matrix(c(NA,4.278748023,-4.237641633,
                         0.626705393,5.351385991,-5.239124508,
                         1.665710587,5.745352531,-4.451685595,
                         5.287617905,-4.854834029,0.916087050))

  expect_equal(quantityIndicator(CES_sigma_2,
                               pvar = "prices",
                               qvar = "quantities",
                               prodID = "prodID",
                               pervar = "time",
                               method = "montgomery"),
               results)

})


