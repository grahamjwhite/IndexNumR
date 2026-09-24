context("bilateral indices")

load(system.file("testdata","testData_bilateral.RData", package = "IndexNumR"))

indexMethods <- c("laspeyres", "paasche", "fisher", "tornqvist", "satovartia",
                  "dutot", "carli", "jevons", "harmonic", "cswd", "walsh", "ces",
                  "geomLaspeyres", "geomPaasche", "tpd", "gk", "palgrave", "drobish",
                  "stuvel", "marshalledgeworth", "lowe", "young")
outputTypes <- c("pop", "chained", "fixedbase")

indexEqual <- function(pOrq, indexMethod, outputType, result){

  switch(pOrq,
         price = expect_equal(priceIndex(CES_sigma_2,
                                         pvar = "prices",
                                         qvar = "quantities",
                                         pervar = "time",
                                         prodID = "prodID",
                                         indexMethod = !!indexMethod,
                                         sample = "matched",
                                         output = !!outputType,
                                         sigma = 2,
                                         weights = "shares"),
                              !!result),
         quantity = eval(bquote(expect_equal(quantityIndex(CES_sigma_2,
                                                           pvar = "prices",
                                                           qvar = "quantities",
                                                           pervar = "time",
                                                           prodID = "prodID",
                                                           indexMethod = .(indexMethod),
                                                           sample = "matched",
                                                           output = .(outputType),
                                                           sigma = 2,
                                                           weights = "shares"),
                                             .(result)))))


}

test_that("bilateral price index functions return the correct values",{
  for(i in seq_along(indexMethods)){
    for(j in seq_along(outputTypes)){

      indexEqual("price",
                 indexMethods[i],
                 outputTypes[j],
                 as.matrix(testData[[paste0(indexMethods[i],"_",outputTypes[j])]]))

    }
  }
})

test_that("error is thrown when wrong column names are given",{
  expect_error(priceIndex(CES_sigma_2,pvar = "price",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "laspeyres",
                          sample="matched", output = "chained"),
               "are not column names of the input data frame")
})

test_that("error is thrown when wrong column names are given",{
  expect_error(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantitie",
                          pervar = "time", prodID = "prodID", indexMethod = "laspeyres",
                          sample = "matched", output = "chained"),
               "are not column names of the input data frame")
})

test_that("error is thrown when wrong index method is specified",{
  expect_error(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "wrong_method",
                          sample="matched", output = "chained"),
               "Not a valid index number method.")
})

test_that("error is thrown when output method is specified",{
  expect_error(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "laspeyres",
                          sample="matched", output = "wrong_output"),
               "Not a valid output type. Please choose from chained, fixedbase or pop.")
})

test_that("Error is thrown when wrong chain method given", {

  expect_error(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "laspeyres",
                          sample="matched", output = "chained", chainMethod = "wrong_method"),
               "Not a valid chainMethod. Please choose from pop, plspread, asymplinear, logquadratic, mixscale, predictedshare")
})

test_that("Error is thrown for an invalid tpd weight type", {
  expect_error(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "laspeyres",
                          sample="matched", output = "chained", weights = "wrong_weights"),
               "Not a valid weight type. Please choose from unweighted, shares or average.")
})

test_that("Error is thrown when time/product combinations are not unique", {
  duped <- CES_sigma_2
  # set the time for the second observation on product 1 to period 1
  # which creates a duplicate time/product combination
  duped$time[2] <- 1
  expect_error(priceIndex(duped, pvar="prices", qvar="quantities", pervar="time", prodID = "prodID"))
})

rm(testData)

#load CES_sigma_2 and make period 3 missing
dat <- CES_sigma_2
dat$time[dat$time==3] <- 2

test_that("error is thrown when a time period is missing",{
  expect_error(priceIndex(dat, pvar="prices",qvar="quantities", pervar = "time",
                          prodID = "prodID", indexMethod = "laspeyres",
                          output = "chained"),
               "The time period variable is not continuous. Missing periods: 3")
})

rm(dat)

load(system.file("testdata","testData_bilateral_quantity.RData",package = "IndexNumR"))

test_that("bilateral quantity index functions return the correct values",{

  for(i in seq_along(indexMethods)){
    for(j in seq_along(outputTypes)){
      indexEqual("quantity",
                 indexMethods[i],
                 outputTypes[j],
                 as.matrix(testData[[paste0(indexMethods[i],"_",outputTypes[j])]]))
    }
  }

})

rm(testData)

# setup a random ordering of the 4 products for 4 periods
v1 <- 1:4
v2 <- rev(v1)
v3 <- c(4,2,1,3)
v4 <- rev(v3)

CESUnordered <- as.data.frame(matrix(NA, nrow = 16, ncol = 4))
# bind the 4 periods of data in the order defined by v1-v4
CESUnordered[1:4,] <- CES_sigma_2[CES_sigma_2$time == 1,][v1,]
CESUnordered[5:8,] <- CES_sigma_2[CES_sigma_2$time == 2,][v2,]
CESUnordered[9:12,] <- CES_sigma_2[CES_sigma_2$time == 3,][v3,]
CESUnordered[13:16,] <- CES_sigma_2[CES_sigma_2$time == 4,][v4,]

colnames(CESUnordered) <- colnames(CES_sigma_2)

CESOrdered <- CES_sigma_2[CES_sigma_2$time %in% 1:4,]

test_that("Indices return the same answer regardless of product ordering",{

  doIndex <- function(dataset, method){
    priceIndex(dataset, pvar = "prices", qvar = "quantities", prodID = "prodID",
               pervar = "time", indexMethod = method)
  }

  indexEqual <- function(dataset1, method, dataset2){
    expect_equal(doIndex(dataset1, method), doIndex(dataset2, method))
  }

  for(i in seq_along(indexMethods)){
    indexEqual(CESOrdered, indexMethods[i], CESUnordered)
  }

})


test_that("bilateral tpd with average weights gives a tornqvist index", {

  tpd <- priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                    prodID = "prodID", indexMethod = "tpd", biasAdjust = FALSE,
                    weights = "average")
  torn <- priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                     prodID = "prodID", indexMethod = "tornqvist")

  expect_equal(tpd, torn)

})

test_that("bilateral unweighted tpd gives a jevons index", {

  tpd <- priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                    prodID = "prodID", indexMethod = "tpd", biasAdjust = FALSE,
                    weights = "unweighted")
  jev <- priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                     prodID = "prodID", indexMethod = "jevons")

  expect_equal(tpd, jev)

})

test_that("bilateral unweighted tpd gives a matched-model jevons index with missing products", {

  tpd <- priceIndex(CES_sigma_2[-15,], pvar = "prices", qvar = "quantities", pervar = "time",
                    prodID = "prodID", indexMethod = "tpd", biasAdjust = FALSE,
                    weights = "unweighted", sample = "")
  jev <- priceIndex(CES_sigma_2[-15,], pvar = "prices", qvar = "quantities", pervar = "time",
                    prodID = "prodID", indexMethod = "jevons", sample = "matched")

  expect_equal(tpd, jev)

})


test_that("The right result is given when prices are imputed", {

  df <- CES_sigma_2[-c(2:3),]
  result <- priceIndex(df, pvar = "prices", qvar = "quantities", pervar = "time",
             prodID = "prodID", indexMethod = "fisher", output = "chained", imputePrices = "carry")

  expect_equal(result, as.matrix(c(1, 0.881280297388098, 1.10748511558337, 1.13268619923687,
                                   0.932248672354066, 1.18881766113942, 1.13162788219576, 0.937414782841657,
                                   1.10719909454772, 0.955793570240181, 1.13893096121809, 1.14473220737899
  )))

})


test_that("misspelled arguments are caught instead of silently ignored (issue #83)", {

  # indexMmethod was absorbed by ... and every method returned the default laspeyres index
  expect_error(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", indexMmethod = "paasche", output = "chained"),
               "Unknown argument\\(s\\) to priceIndex: indexMmethod \\(did you mean indexMethod\\?\\)")

  expect_error(quantityIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                             prodID = "prodID", indexMmethod = "paasche", output = "chained"),
               "Unknown argument\\(s\\) to priceIndex: indexMmethod")

  # arguments for mixScaleDissimilarity still work with mixscale chaining
  expect_equal(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", output = "chained", chainMethod = "mixscale",
                          measure = "mix"),
               priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", output = "chained", chainMethod = "mixScale",
                          measure = "mix"))

  # but are an error when they would be ignored
  expect_error(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", output = "chained", chainMethod = "pop",
                          measure = "mix"),
               "only used when output = \"chained\" and chainMethod = \"mixscale\"")
})


test_that("argument values are not case sensitive", {

  expect_equal(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", indexMethod = "Lowe"),
               priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", indexMethod = "lowe"))

  expect_equal(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", indexMethod = "Young", output = "Chained"),
               priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", indexMethod = "young", output = "chained"))
})


test_that("quantityIndex passes imputePrices and loweYoungBase to the calculation", {

  dm <- CES_sigma_2[-c(1, 2),]

  imputed <- quantityIndex(dm, pvar = "prices", qvar = "quantities", pervar = "time",
                           prodID = "prodID", indexMethod = "fisher", output = "chained",
                           imputePrices = "carry")
  expected <- quantityIndex(imputeCarryPrices(dm, "prices", "quantities", "time", "prodID"),
                            pvar = "prices", qvar = "quantities", pervar = "time",
                            prodID = "prodID", indexMethod = "fisher", output = "chained")
  notImputed <- quantityIndex(dm, pvar = "prices", qvar = "quantities", pervar = "time",
                              prodID = "prodID", indexMethod = "fisher", output = "chained")

  expect_equal(imputed, expected)
  expect_false(isTRUE(all.equal(imputed, notImputed)))

  base1 <- quantityIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                         prodID = "prodID", indexMethod = "lowe", loweYoungBase = 1)
  base3 <- quantityIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                         prodID = "prodID", indexMethod = "lowe", loweYoungBase = 3)
  expect_false(isTRUE(all.equal(base1, base3)))
})


test_that("basePeriod only affects fixed base indexes", {

  pop1 <- priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                     prodID = "prodID", output = "pop")
  pop3 <- expect_silent(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities",
                                   pervar = "time", prodID = "prodID", output = "pop",
                                   basePeriod = 3))
  expect_equal(pop1, pop3)

  fb3 <- priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                    prodID = "prodID", output = "fixedbase", basePeriod = 3)
  expect_equal(fb3[3, 1], 1)

  expect_error(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", output = "fixedbase", basePeriod = 13),
               "basePeriod must be one of the time periods")
})


test_that("an unmatched sample with different products is an error, not a wrong answer", {

  # product 1 leaves in period 2 and product 5 enters
  dx <- CES_sigma_2[!(CES_sigma_2$time == 2 & CES_sigma_2$prodID == 1),]
  dx <- rbind(dx, data.frame(time = 2, prices = 5, quantities = 1, prodID = 5))

  expect_error(priceIndex(dx, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", sample = "unmatched", indexMethod = "laspeyres"),
               "The products differ between the periods compared for period 2")

  # with the same products in every period, unmatched gives the same result as matched
  expect_equal(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", sample = "unmatched", indexMethod = "fisher"),
               priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", sample = "matched", indexMethod = "fisher"))

  expect_error(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", sample = "match"),
               "Not a valid sample type")
})


test_that("Sato-Vartia index is defined when shares do not change", {

  dx <- CES_sigma_2[CES_sigma_2$time %in% 1:2,]
  dx$prices[dx$time == 2] <- dx$prices[dx$time == 1]*1.1
  dx$quantities[dx$time == 2] <- dx$quantities[dx$time == 1]

  expect_equal(priceIndex(dx, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", indexMethod = "satovartia")[2, 1], 1.1)
})


test_that("bilateral TPD index works with a single matched product", {

  dx <- CES_sigma_2[CES_sigma_2$prodID == 1 | CES_sigma_2$time > 2,]

  result <- priceIndex(dx, pvar = "prices", qvar = "quantities", pervar = "time",
                       prodID = "prodID", indexMethod = "tpd")

  expect_equal(result[2, 1], 1.75/2)
})


test_that("quantity-based methods require a quantity variable", {

  expect_error(priceIndex(CES_sigma_2, pvar = "prices", qvar = "", pervar = "time",
                          prodID = "prodID", indexMethod = "laspeyres"),
               "A quantity variable is required for the laspeyres index method")

  expect_error(priceIndex(CES_sigma_2, pvar = "prices", qvar = "", pervar = "time",
                          prodID = "prodID", indexMethod = "jevons", output = "chained",
                          chainMethod = "logquadratic"),
               "A quantity variable is required for chainMethod = \"logquadratic\"")

  expect_silent(priceIndex(CES_sigma_2, pvar = "prices", qvar = "", pervar = "time",
                           prodID = "prodID", indexMethod = "jevons", output = "chained",
                           chainMethod = "predictedshare"))
})


test_that("the time period variable must start at 1", {

  dx <- CES_sigma_2
  dx$time <- dx$time + 1

  expect_error(priceIndex(dx, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID"),
               "The time period variable must start at 1")
})
