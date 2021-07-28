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

test_that("error is thrown when wrong column names are given",{
  expect_error(priceIndex(CES_sigma_2, pvar = "prices", qvar = "quantitie",
                          pervar = "time", prodID = "prodID", indexMethod = "laspeyres",
                          sample = "matched", output = "chained"),
               "are not column names of the input data frame")
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
