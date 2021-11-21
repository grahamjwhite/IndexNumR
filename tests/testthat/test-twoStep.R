test_that("two-step index function works with bilateral indexes", {

  #create a df with groups
  df <- CES_sigma_2
  df$group <- ifelse(df$prodID > 2, 2, 1)

  argsList <- list(x = df, pvar = "prices", qvar = "quantities", prodID = "prodID",
                   pervar = "time", indexMethod = "laspeyres", output = "chained",
                   chainMethod = "pop", sample = "matched")

  twoStep <- twoStepIndex(df, "prices", "quantities", "time", "prodID", "group",
                          stepOneArgs = argsList, stepTwoArgs = argsList, output = "chained"
                          #stepOneFunction = priceIndex, stepTwoFunction = priceIndex
                          )
  oneStep <- priceIndex(df, "prices", "quantities", "time", prodID = "prodID", output = 'chained')

  # when a laspeyres index is used, these should be equal
  expect_equal(oneStep, twoStep)

  # also with a Paasche, these should be equal
  argsList$indexMethod <- "paasche"
  argsList$output <- "chained"

  twoStep <- twoStepIndex(df, "prices", "quantities", "time", "prodID", "group",
                          stepOneArgs = argsList, stepTwoArgs = argsList, output = "chained"
                          #stepOneFunction = priceIndex, stepTwoFunction = priceIndex
                          )
  oneStep <- priceIndex(df, "prices", "quantities", "time", prodID = "prodID", output = 'chained',
                        indexMethod = "paasche")

  expect_equal(oneStep, twoStep)

  # repeat for a fixed base Paasche index
  argsList$indexMethod <- "paasche"
  argsList$output <- "fixedbase"

  twoStep <- twoStepIndex(df, "prices", "quantities", "time", "prodID", "group",
                          stepOneArgs = argsList,stepTwoArgs = argsList, output = "fixedbase"
                          #stepOneFunction = priceIndex, stepTwoFunction = priceIndex
                          )
  oneStep <- priceIndex(df, "prices", "quantities", "time", prodID = "prodID", output = 'fixedbase',
                        indexMethod = "paasche")

  expect_equal(oneStep, twoStep)

  # and a fixed base Laspeyres
  argsList$indexMethod <- "laspeyres"
  argsList$output <- "fixedbase"

  twoStep <- twoStepIndex(df, "prices", "quantities", "time", "prodID", "group",
                          stepOneArgs = argsList,stepTwoArgs = argsList, output = "fixedbase"
                          #stepOneFunction = priceIndex, stepTwoFunction = priceIndex
                          )
  oneStep <- priceIndex(df, "prices", "quantities", "time", prodID = "prodID", output = 'fixedbase',
                        indexMethod = "laspeyres")

  expect_equal(oneStep, twoStep)

})


test_that("two-step index function is the same as one step with product change", {

  #create a df with groups
  df <- CES_sigma_2[-c(14,33:36),]
  df$group <- ifelse(df$prodID > 2, 2, 1)

  argsList <- list(x = df, pvar = "prices", qvar = "quantities", prodID = "prodID",
                   pervar = "time", indexMethod = "laspeyres", output = "chained",
                   chainMethod = "pop", sample = "matched")

  twoStep <- twoStepIndex(df, "prices", "quantities", "time", "prodID", "group",
                          stepOneArgs = argsList, stepTwoArgs = argsList, output = "chained"
                          #stepOneFunction = priceIndex, stepTwoFunction = priceIndex
                          )
  oneStep <- priceIndex(df, "prices", "quantities", "time", prodID = "prodID", output = 'chained')

  # when a laspeyres index is used, these should be equal
  expect_equal(oneStep, twoStep)

  # also with a Paasche, these should be equal
  argsList$indexMethod <- "paasche"
  argsList$output <- "chained"

  twoStep <- twoStepIndex(df, "prices", "quantities", "time", "prodID", "group",
                          stepOneArgs = argsList, stepTwoArgs = argsList, output = "chained"
                          #stepOneFunction = priceIndex, stepTwoFunction = priceIndex
                          )
  oneStep <- priceIndex(df, "prices", "quantities", "time", prodID = "prodID", output = 'chained',
                        indexMethod = "paasche")

  expect_equal(oneStep, twoStep)

  # repeat for a fixed base Paasche index
  argsList$indexMethod <- "paasche"
  argsList$output <- "fixedbase"

  twoStep <- twoStepIndex(df, "prices", "quantities", "time", "prodID", "group",
                          stepOneArgs = argsList, stepTwoArgs = argsList, output = "fixedbase"
                          #stepOneFunction = priceIndex, stepTwoFunction = priceIndex
                          )
  oneStep <- priceIndex(df, "prices", "quantities", "time", prodID = "prodID", output = 'fixedbase',
                        indexMethod = "paasche")

  expect_equal(oneStep, twoStep)

  # and a fixed base Laspeyres
  argsList$indexMethod <- "laspeyres"
  argsList$output <- "fixedbase"

  twoStep <- twoStepIndex(df, "prices", "quantities", "time", "prodID", "group",
                          stepOneArgs = argsList, stepTwoArgs = argsList, output = "fixedbase"
                          #stepOneFunction = priceIndex, stepTwoFunction = priceIndex
                          )
  oneStep <- priceIndex(df, "prices", "quantities", "time", prodID = "prodID", output = 'fixedbase',
                        indexMethod = "laspeyres")

  expect_equal(oneStep, twoStep)

})


test_that("sub group indexes are calculated correctly for bilateral index methods", {

  #create a df with groups
  df <- CES_sigma_2
  df$group <- ifelse(df$prodID > 2, 2, 1)

  argsList <- list(x = df, pvar = "prices", qvar = "quantities", prodID = "prodID",
                   pervar = "time", indexMethod = "laspeyres", output = "chained",
                   chainMethod = "pop", sample = "matched")

  subIndexes <- subgroupPriceIndexes(subgroup = "group", indexFunction = priceIndex,
                                     indexArgs = argsList)

  dfOne <- df[df$group == 1,]
  dfTwo <- df[df$group == 2,]

  groupOnePrice <- priceIndex(dfOne, "prices", "quantities", "time", prodID = "prodID",
                              output = "chained", indexMethod = "laspeyres", sample = "matched",
                              chainMethod = "pop")

  groupTwoPrice <- priceIndex(dfTwo, "prices", "quantities", "time", prodID = "prodID",
                              output = "chained", indexMethod = "laspeyres", sample = "matched",
                              chainMethod = "pop")

  expect_equal(groupOnePrice, as.matrix(subIndexes[[1]]$prices))
  expect_equal(groupTwoPrice, as.matrix(subIndexes[[2]]$prices))

})


test_that("sub group indexes are calculated correctly for GEKS method", {

  #create a df with groups
  df <- CES_sigma_2
  df$group <- ifelse(df$prodID > 2, 2, 1)

  argsList <- list(x = df, pvar = "prices", qvar = "quantities", prodID = "prodID",
                   pervar = "time", indexMethod = "fisher", sample = "matched", window = 12)

  subIndexes <- subgroupPriceIndexes(subgroup = "group", indexFunction = GEKSIndex,
                                     indexArgs = argsList)

  dfOne <- df[df$group == 1,]
  dfTwo <- df[df$group == 2,]

  groupOnePrice <- GEKSIndex(dfOne, "prices", "quantities", "time", prodID = "prodID",
                             indexMethod = "fisher", sample = "matched", window = 12)

  groupTwoPrice <- GEKSIndex(dfTwo, "prices", "quantities", "time", prodID = "prodID",
                             indexMethod = "fisher", sample = "matched", window = 12)

  expect_equal(groupOnePrice, as.matrix(subIndexes[[1]]$prices))
  expect_equal(groupTwoPrice, as.matrix(subIndexes[[2]]$prices))

})

