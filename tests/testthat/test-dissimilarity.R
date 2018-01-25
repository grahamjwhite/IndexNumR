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
})

rm(testData)
