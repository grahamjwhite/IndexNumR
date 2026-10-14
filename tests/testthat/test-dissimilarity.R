context("dissimilarity measures")

load(system.file("testdata","testData_dissimilarity.RData",package = "IndexNumR"))

test_that("Dissimilarity measures return the correct values",{
  expect_equal(relativeDissimilarity(CES_sigma_2, pvar="prices",qvar="quantities",
                                     pervar="time",prodID = "prodID",indexMethod = "fisher",
                                     similarityMethod = "logquadratic"), testData$lq)
  expect_equal(relativeDissimilarity(CES_sigma_2, pvar="prices",qvar="quantities",
                                     pervar="time",prodID = "prodID",indexMethod = "fisher",
                                     similarityMethod = "asymplinear"), testData$ab)
  expect_equal(relativeDissimilarity(CES_sigma_2, pvar="prices",qvar="quantities",
                                     pervar="time",prodID = "prodID",indexMethod = "fisher",
                                     similarityMethod = "plspread"), testData$pl)
  expect_equal(mixScaleDissimilarity(CES_sigma_2, pvar="prices",qvar="quantities",pervar="time",
                                     prodID = "prodID",measure = "absolute", combine = "geomean"),
               testData$abs)
  expect_equal(mixScaleDissimilarity(CES_sigma_2, pvar="prices",qvar="quantities",pervar="time",
                                     prodID = "prodID",measure = "mix", combine = "geomean"),
               testData$mix)
  expect_equal(mixScaleDissimilarity(CES_sigma_2, pvar="prices",qvar="quantities",pervar="time",
                                     prodID = "prodID",measure = "scale", combine = "geomean"),
               testData$scale)
  expect_equal(relativeDissimilarity(CES_sigma_2, pvar="prices",qvar="quantities",pervar="time",
                                     prodID = "prodID", similarityMethod = "predictedshare"),
               testData$predictedShare)
  expect_equal(predictedShareDissimilarity(CES_sigma_2, pvar="prices", qvar="quantities", pervar="time",
                                     prodID = "prodID"), testData$predictedShare)
})

test_that("maximum similarity links are estimated correctly",{
  expect_equal(maximumSimilarityLinks(testData$lq), testData$maxlinks)
})

test_that("similarity linked bilateral price index functions return the correct values",{
  expect_equal(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",
                          prodID = "prodID",indexMethod = "laspeyres", sample="matched",
                          output = "chained", chainMethod = "logquadratic"), testData$p_lq)
  expect_equal(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",
                          prodID = "prodID",indexMethod = "laspeyres", sample="matched",
                          output = "chained", chainMethod = "asymplinear"), testData$p_ab)
  expect_equal(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",
                          prodID = "prodID",indexMethod = "laspeyres", sample="matched",
                          output = "chained", chainMethod = "plspread"), testData$p_pl)
  expect_equal(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",
                          prodID = "prodID",indexMethod = "laspeyres", sample="matched",
                          output = "chained", chainMethod = "mixscale", measure="absolute")
               , testData$p_abs)
  expect_equal(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",
                          prodID = "prodID",indexMethod = "laspeyres", sample="matched",
                          output = "chained", chainMethod = "mixscale", measure="mix")
               , testData$p_mix)
  expect_equal(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",
                          prodID = "prodID",indexMethod = "laspeyres", sample="matched",
                          output = "chained", chainMethod = "mixscale", measure="scale")
               , testData$p_scale)
  expect_equal(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",
                          prodID = "prodID",indexMethod = "laspeyres", sample="matched",
                          output = "chained", chainMethod = "predictedshare"), testData$p_predictedShare)
})

rm(testData)


# shuffle the rows so that products are in a different order in each period
set.seed(1)
CESShuffled <- CES_sigma_2[sample(nrow(CES_sigma_2)),]

test_that("dissimilarity measures do not depend on the row order of the data", {

  expect_equal(relativeDissimilarity(CESShuffled, pvar = "prices", qvar = "quantities",
                                     pervar = "time", prodID = "prodID",
                                     similarityMethod = "logquadratic"),
               relativeDissimilarity(CES_sigma_2, pvar = "prices", qvar = "quantities",
                                     pervar = "time", prodID = "prodID",
                                     similarityMethod = "logquadratic"))

  expect_equal(mixScaleDissimilarity(CESShuffled, pvar = "prices", qvar = "quantities",
                                     pervar = "time", prodID = "prodID"),
               mixScaleDissimilarity(CES_sigma_2, pvar = "prices", qvar = "quantities",
                                     pervar = "time", prodID = "prodID"))

  expect_equal(relativeDissimilarity(CES_sigma_2, pvar = "prices", qvar = "quantities",
                                     pervar = "time", prodID = "prodID",
                                     similarityMethod = "PredictedShare"),
               relativeDissimilarity(CES_sigma_2, pvar = "prices", qvar = "quantities",
                                     pervar = "time", prodID = "prodID",
                                     similarityMethod = "predictedshare"))
})

rm(CESShuffled)


test_that("maximumSimilarityLinks uses the period, not the row position", {

  lq <- relativeDissimilarity(CES_sigma_2, pvar = "prices", qvar = "quantities",
                              pervar = "time", prodID = "prodID")

  expect_equal(maximumSimilarityLinks(lq[nrow(lq):1,]), maximumSimilarityLinks(lq))
})


test_that("maximumSimilarityLinks works with two periods", {

  lq <- relativeDissimilarity(CES_sigma_2[CES_sigma_2$time <= 2,], pvar = "prices",
                              qvar = "quantities", pervar = "time", prodID = "prodID")

  links <- maximumSimilarityLinks(lq)

  expect_equal(nrow(links), 2)
  expect_equal(links$x0, c(1, 1))
})
