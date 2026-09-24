
test_that("imputeCarryPrices carrys backward prices", {

  testDat <- CES_sigma_2[CES_sigma_2$time < 4,]
  testDat <- testDat[-(1:2),]

  expected <- data.frame(time = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
                         prices = c(1.6, 1, 1, 0.5, 1.6, 0.5, 0.95, 0.55, 1.6, 1.05, 0.9, 0.6),
                         quantities = c(0, 1.53846153846154, 1.53846153846154, 12.3076923076923,
                                        0, 7.16211636226699, 1.9839657513205, 11.8382088632512,
                                        0.713550247116969, 1.65686043775006, 2.25517115138202, 10.1482701812191),
                         prodID = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4))

  expect_equal(imputeCarryPrices(testDat, "prices", "quantities", "time", "prodID"),
               expected)
})


test_that("imputeCarryPrices carrys forward prices", {

  testDat <- CES_sigma_2[CES_sigma_2$time < 4,]
  testDat <- testDat[-(2:3),]

  expected <- data.frame(time = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
                         prices = c(2, 1, 1, 0.5, 2, 0.5, 0.95, 0.55, 2, 1.05, 0.9, 0.6),
                         quantities = c(0.384615384615385, 1.53846153846154,
                                        1.53846153846154, 12.3076923076923, 0, 7.16211636226699,
                                        1.9839657513205, 11.8382088632512, 0, 1.65686043775006, 2.25517115138202,
                                        10.1482701812191),
                         prodID = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4))

  expect_equal(imputeCarryPrices(testDat, "prices", "quantities", "time", "prodID"),
               expected)
})


test_that("imputeQuantities calculates the right quantities", {

  q <- imputeQuantities(CES_sigma_2, "prices", "time", "prodID")

  expect_equal(q$quantities,
               c(2.0000000000,	4.0000000000,	4.0000000000,	8.0000000000,
               2.2857142857,	8.0000000000,	4.2105263158,	7.2727272727,
               2.5000000000,	3.8095238095,	4.4444444444,	6.6666666667,
               2.6666666667,	3.6363636364,	4.7058823529,	6.1538461538,
               2.7586206897,	3.5714285714,	10.0000000000,	5.7142857143,
               2.8571428571,	3.4782608696,	5.0000000000,	5.3333333333,
               2.9629629630,	3.3898305085,	5.3333333333,	5.7142857143,
               3.0769230769,	6.6666666667,	5.5555555556,	6.1538461538,
               3.2000000000,	3.3333333333,	5.7142857143,	5.7142857143,
               3.3333333333,	3.2000000000,	10.0000000000,	5.3333333333,
               3.4782608696,	3.1250000000,	5.7142857143,	5.3333333333,
               3.6363636364,	3.0769230769,	6.1538461538,	5.0000000000)
  )

})


test_that("imputeCarryPrices keeps factor product identifiers", {

  dx <- CES_sigma_2[CES_sigma_2$time < 4,]
  dx$prodID <- factor(paste0("p", dx$prodID))
  dx <- dx[-(1:2),]

  result <- expect_silent(imputeCarryPrices(dx, "prices", "quantities", "time", "prodID"))

  expect_true(is.factor(result$prodID))
  expect_equal(as.vector(table(result$prodID)), c(3, 3, 3, 3))
  expect_equal(result$prices[result$prodID == "p1"], c(1.6, 1.6, 1.6))
})


test_that("imputeCarryPrices carries prices forward over gaps and keeps other columns", {

  # product 2 is missing in periods 3 to 5, product 3 in period 1
  dx <- CES_sigma_2[CES_sigma_2$time <= 6,]
  dx <- dx[!(dx$prodID == 2 & dx$time %in% 3:5) & !(dx$prodID == 3 & dx$time == 1),]
  dx$group <- ifelse(dx$prodID <= 2, "a", "b")

  result <- imputeCarryPrices(dx, "prices", "quantities", "time", "prodID")

  expect_equal(names(result), names(dx))
  expect_equal(nrow(result), 24)

  p2 <- CES_sigma_2$prices[CES_sigma_2$prodID == 2]
  expect_equal(result$prices[result$prodID == 2], c(p2[1:2], rep(p2[2], 3), p2[6]))
  expect_equal(result$quantities[result$prodID == 2 & result$time %in% 3:5], c(0, 0, 0))

  p3 <- CES_sigma_2$prices[CES_sigma_2$prodID == 3]
  expect_equal(result$prices[result$prodID == 3 & result$time == 1], p3[2])

  # observed rows keep their group, imputed rows have a missing group
  expect_true(all(is.na(result$group[result$prodID == 2 & result$time %in% 3:5])))
  expect_equal(result$group[result$prodID == 2 & result$time == 2], "a")
})
