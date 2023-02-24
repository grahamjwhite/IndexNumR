context("indicator tests")

test_that("price indicator fails nicely on incorrect methods", {

  expect_error(priceIndicator(CES_sigma_2,
                              pvar = "prices",
                              qvar = "quantities",
                              prodID = "prodID",
                              pervar = "time",
                              method = "wrong method"),
               "Invalid method chosen")

})

test_that("price indicator fails nicely on bad column names", {

  expect_error(priceIndicator(CES_sigma_2,
                              pvar = "prices",
                              qvar = "bad column",
                              prodID = "prodID",
                              pervar = "time",
                              method = "laspeyres"),
               "are not column names of the input data frame")

})

test_that("error is thrown when a time period is missing",{

  dat <- CES_sigma_2
  dat$time[dat$time==3] <- 2

  expect_error(priceIndicator(dat,
                          pvar="prices",
                          qvar="quantities",
                          pervar = "time",
                          prodID = "prodID",
                          method = "laspeyres"),
               "The time period variable is not continuous. Missing periods: 3")
})

test_that("Error is thrown when time/product combinations are not unique", {
  duped <- CES_sigma_2
  # set the time for the second observation on product 1 to period 1
  # which creates a duplicate time/product combination
  duped$time[2] <- 1
  expect_error(priceIndicator(duped, pvar="prices", qvar="quantities", pervar="time", prodID = "prodID",
                              method = "laspeyres"))
})

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


test_that("value decomposition returns the right results for Paasche price method", {

  expected <- data.frame(
    price = c(NA,-3.23451166558322,1.19889565518684,
              0.338354798691534,-5.65501232619842,1.89061743778933,
              -0.724047978536408,-4.76683535762483,1.38856453281232,
              -4.49985294117647,1.65051935084982,0.0250350184983226),
    quantity = c(NA,6.23451166558322,-3.19889565518684,
                 0.661645201308465,8.65501232619842,-3.89061743778933,
                 1.72404797853641,7.76683535762483,-3.38856453281232,7.49985294117647,
                 -3.65051935084981,0.974964981501678),
    changes = c(NA, 3, -2, 0.999999999999998, 3, -2, 1, 3, -2, 3, -2, 1),
    values = c(NA, 13, 11, 12, 15, 13, 14, 17, 15, 18, 16, 17)
  )

  result <- valueDecomposition(CES_sigma_2,
                               pvar = "prices",
                               qvar = "quantities",
                               prodID = "prodID",
                               pervar = "time",
                               priceMethod = "paasche")

  expect_equal(expected, result)

})

test_that("value decomposition returns the right results for laspeyres price method", {

  expected <- data.frame(
    price = c(NA,-0.326923076923076,4.34417677081562,
              0.406142948667661,-0.806657971531005,5.84513819985051,
              -0.611482951157069,-1.69927459763315,4.52035537561853,
              -1.02746523388116,4.92214705882353,0.139606945104282),
    quantity = c(NA,3.32692307692308,-6.34417677081562,
                 0.593857051332339,3.80665797153101,-7.84513819985052,
                 1.61148295115707,4.69927459763315,-6.52035537561853,4.02746523388116,
                 -6.92214705882353,0.860393054895719),
    changes = c(NA, 3, -2, 0.999999999999998, 3, -2, 1, 3, -2, 3, -2, 1),
    values = c(NA, 13, 11, 12, 15, 13, 14, 17, 15, 18, 16, 17)
  )

  result <- valueDecomposition(CES_sigma_2,
                               pvar = "prices",
                               qvar = "quantities",
                               prodID = "prodID",
                               pervar = "time",
                               priceMethod = "laspeyres")

  expect_equal(expected, result)

})


test_that("value decomposition returns the right results for bennet price method", {

  expected <- data.frame(
    price = c(NA,-1.78071737125315,2.77153621300123,
              0.372248873679598,-3.23083514886471,3.86787781881992,
              -0.667765464846739,-3.23305497762899,2.95445995421543,
              -2.76365908752882,3.28633320483667,0.0823209818013025),
    quantity = c(NA,4.78071737125315,-4.77153621300123,
                 0.627751126320402,6.23083514886471,-5.86787781881992,
                 1.66776546484674,6.23305497762899,-4.95445995421543,5.76365908752881,
                 -5.28633320483667,0.917679018198699),
    changes = c(NA, 3, -2, 0.999999999999998, 3, -2, 1, 3, -2, 3, -2, 1),
    values = c(NA, 13, 11, 12, 15, 13, 14, 17, 15, 18, 16, 17)
  )

  result <- valueDecomposition(CES_sigma_2,
                               pvar = "prices",
                               qvar = "quantities",
                               prodID = "prodID",
                               pervar = "time",
                               priceMethod = "bennet")

  expect_equal(expected, result)

})


test_that("value decomposition returns the right results for montgomery price method", {

  expected <- data.frame(
    price = c(NA,-1.27874802287663,2.23764163264406,
              0.373294606860997,-2.35138599056262,3.23912450758093,
              -0.665710587242193,-2.74535253122247,2.45168559478284,
              -2.2876179051576,2.85483402898994,0.0839129500556),
    quantity = c(NA,4.27874802287663,-4.23764163264406,
                 0.626705393139003,5.35138599056262,-5.23912450758093,
                 1.66571058724219,5.74535253122247,-4.45168559478284,5.2876179051576,
                 -4.85483402898994,0.916087049944403),
    changes = c(NA, 3, -2, 0.999999999999998, 3, -2, 1, 3, -2, 3, -2, 1),
    values = c(NA, 13, 11, 12, 15, 13, 14, 17, 15, 18, 16, 17)
  )


  result <- valueDecomposition(CES_sigma_2,
                               pvar = "prices",
                               qvar = "quantities",
                               prodID = "prodID",
                               pervar = "time",
                               priceMethod = "montgomery")

  expect_equal(expected, result)

})
