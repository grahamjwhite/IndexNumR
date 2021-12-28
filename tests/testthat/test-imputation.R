
test_that("imputeCarryPrices carrys backward prices", {

  testDat <- CES_sigma_2[CES_sigma_2$time < 4,]
  testDat <- testDat[-(1:2),]

  expected <- data.frame(time = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
                         prices = c(1.6, 1, 1, 0.5, 1.6, 0.5, 0.95, 0.55, 1.6, 1.05, 0.9, 0.6),
                         quantities = c(0, 1.53846153846154, 1.53846153846154, 12.3076923076923,
                                        0, 7.16211636226699, 1.9839657513205, 11.8382088632512,
                                        0.713550247116969, 1.65686043775006, 2.25517115138202, 10.1482701812191),
                         prodID = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
                         row.names = c(4L, 2L, 3L, 1L, 8L, 5L, 7L, 6L, 12L, 11L, 10L, 9L))

  expect_equal(imputeCarryPrices(testDat, "prices", "quantities", "time", "prodID"),
               expected)
})


test_that("imputeCarryPrices carrys forward prices", {

  testDat <- CES_sigma_2[CES_sigma_2$time < 4,]
  testDat <- testDat[-(2:3),]

  expected <- data.frame(time = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
                         prices = c(1.6, 1, 1, 0.5, 1.6, 0.5, 0.95, 0.55, 1.6, 1.05, 0.9, 0.6),
                         quantities = c(0.384615384615385, 1.53846153846154, 1.53846153846154, 12.3076923076923,
                                        0.584662560185061, 7.16211636226699, 1.9839657513205, 11.8382088632512,
                                        0.713550247116969, 1.65686043775006, 2.25517115138202, 10.1482701812191),
                         prodID = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4))

  expect_equal(imputeCarryPrices(testDat, "prices", "quantities", "time", "prodID"),
               expected)
})
