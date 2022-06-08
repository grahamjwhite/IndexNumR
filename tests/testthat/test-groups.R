test_that("group indexes calculate correctly", {

  df <- CES_sigma_2
  df$group <- c(rep(1, 24), rep(2, 24))

  # bilateral index
  result <- groupIndexes("group", "priceIndex", list(x = df, pvar = "prices",
                                                   qvar = "quantities", pervar = "time",
                                                   prodID = "prodID", indexMethod = "fisher",
                                                   output = "chained"))

  expected <- list(data.frame(prices = c(1, 0.587702932477034, 0.938040539261391, 0.93897888631988,
                                         0.934968720757713, 0.934175479864956, 0.931616036677247, 0.611250809270594,
                                         0.903982763659577, 0.903982763659577, 0.894410922698385, 0.879753796751627),
                              time = 1:12,
                              group = 1),
                   data.frame(prices = c(1, 1.06616532780994, 1.12468016473048, 1.17505874635192, 0.918863344028904,
                                         1.26412951284683, 1.18153014538001, 1.10868556021176, 1.15522770440989,
                                         0.953355437705252, 1.20684257519461, 1.22370774351111),
                              time = 1:12,
                              group = 2))

  expect_equal(result, expected)


  # multilateral index
  result <- groupIndexes("group", "GEKSIndex", list(x = df, pvar = "prices",
                                                  qvar = "quantities", pervar = "time",
                                                  prodID = "prodID", indexMethod = "fisher",
                                                  window = 12))

  expected <- list(data.frame(prices = c(1, 0.600833224111901, 0.94694540492366,
                                             0.946847170171006, 0.942334986942289, 0.940985850715345, 0.937850979918209,
                                             0.617043784922898, 0.911112714154517, 0.910423631621593, 0.900282767810343,
                                             0.885157225454677),
                                  time = 1:12,
                                  group = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                   data.frame(prices = c(1, 1.05858419895244, 1.11149810360747,
                                             1.15775514194229, 0.904036621624117, 1.25315743405227, 1.1713514350302,
                                             1.09967141747193, 1.14415689292318, 0.935498131983926, 1.19609672061236,
                                             1.20991604196587),
                                  time = 1:12,
                                  group = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)))

  expect_equal(result, expected)

})


test_that("groupIndexes parameter validation", {

  argsList <- list(x = CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                   prodID = "prodID", indexMethod = "fisher", output = "chained")

  expect_error(groupIndexes("groupID", "priceIndex", argsList),
               "groupID is not a column name of the data frame given in indexArgs")

  df <- CES_sigma_2
  df$groupID <- c(rep(1, 24), rep(2, 24))

  argsList <- list(x = df, pvar = "prices", qvar = "quantities", pervar = "time",
                   prodID = "prodID", indexMethod = "fisher", output = "chained")

  expect_error(groupIndexes("groupID", "evaluateMatched", argsList),
               "Invalid or incorrect indexFunction given. Valid functions are: priceIndex, quantityIndex, GEKSIndex, GKIndex, WTPDIndex, priceIndicator")


})


test_that("year-over-year indexes return the correct results", {

  # quarterly
  result <- yearOverYearIndexes("quarterly", "priceIndex", list(x = CES_sigma_2, pvar = "prices",
                                                              qvar = "quantities", pervar = "time",
                                                              prodID = "prodID", indexMethod = "fisher",
                                                              output = "chained"))

  expected <- list(structure(list(prices = c(1, 0.892040981345701, 1.05271310844063),
                                  time = c(1, 2, 3),
                                  quarter = c(1L, 1L, 1L)),
                             class = "data.frame",
                             row.names = c(NA, -3L)),
                   structure(list(prices = c(1, 1.32412421462851, 1.05883554975549),
                                  time = c(1, 2, 3),
                                  quarter = c(2L, 2L, 2L)),
                             class = "data.frame",
                             row.names = c(NA, -3L)),
                   structure(list(prices = c(1, 1.04075907097092, 1.04637894626968),
                                  time = c(1, 2, 3),
                                  quarter = c(3L, 3L, 3L)),
                             class = "data.frame",
                             row.names = c(NA, -3L)),
                   structure(list(prices = c(1, 0.838891085148608, 1.02467001948107),
                                  time = c(1, 2, 3),
                                  quarter = c(4L, 4L, 4L)),
                             class = "data.frame",
                             row.names = c(NA, -3L)))

  expect_equal(result, expected)


  # monthly
  # we need some data we can treat as monthly
  df1 <- CES_sigma_2
  df2 <- CES_sigma_2
  df2$time <- df2$time + 12
  df2$prices <- df2$prices * 1.1
  df3 <- CES_sigma_2
  df3$time <- df3$time + 24
  df3$prices <- df3$prices * 1.2

  df <- do.call(rbind, list(df1, df2, df3))

  result <- yearOverYearIndexes("monthly", "priceIndex", list(x = df, pvar = "prices",
                                                  qvar = "quantities", pervar = "time",
                                                  prodID = "prodID", indexMethod = "fisher",
                                                  output = "chained"))

  expected <- lapply(1:12, function(x){data.frame(prices = c(1, 1.1, 1.2),
                                                    time = 1:3,
                                                    month = x)})
  expect_equal(result, expected)

})


test_that("yearOverYearIndexes parameter validation", {

  argsList <- list(x = CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                   prodID = "prodID", indexMethod = "fisher", output = "chained")

  expect_error(yearOverYearIndexes("wrong freq", "priceIndexs", args),
               "Incorrect freq argument. Must be 'monthly' or 'quarterly'.")

  expect_error(yearOverYearIndexes("quarterly", "evaluateMatched", args),
               "Invalid or incorrect indexFunction given. Valid functions are: priceIndex, quantityIndex, GEKSIndex, GKIndex, WTPDIndex, priceIndicator")

})


