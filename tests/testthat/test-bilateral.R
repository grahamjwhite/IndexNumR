context("bilateral indices")

load(system.file("testdata","testData_bilateral.RData",package = "IndexNumR"))

indexMethods <- c("laspeyres","paasche","fisher","tornqvist","satovartia",
                  "dutot","carli","jevons","harmonic","cswd","walsh","ces")
outputTypes <- c("pop","chained","fixedbase")

for(i in seq_along(indexMethods)){
  for(j in seq_along(outputTypes)){
    test_that("bilateral price index functions return the correct values",{
      expect_equal(priceIndex(CES_sigma_2,pvar="prices",qvar="quantities",
                              pervar="time",prodID = "prodID",
                              indexMethod = indexMethods[i],
                              sample = "matched",output=outputTypes[j],
                              sigma=2),
                   as.matrix(testData[[paste0(indexMethods[i],"_",outputTypes[j])]]))
    })
  }
}

test_that("error is thrown when wrong column names are given",{
  expect_error(priceIndex(CES_sigma_2,pvar = "price",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "laspeyres",
                          sample="matched", output = "chained"),
               "are not column names of the input data frame")
})

test_that("error is thrown when wrong index method is specified",{
  expect_error(priceIndex(CES_sigma_2,pvar = "price",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "wrong_method",
                          sample="matched", output = "chained"),
               "Not a valid index number method.")
})

test_that("error is thrown when output method is specified",{
  expect_error(priceIndex(CES_sigma_2,pvar = "price",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "laspeyres",
                          sample="matched", output = "wrong_output"),
               "Not a valid output type. Please choose from chained, fixedbase or pop.")
})

rm(testData)

#load CES_sigma_2 and make period 3 missing
dat <- CES_sigma_2
dat$time[dat$time==3] = 2

test_that("error is thrown when a time period is missing",{
  expect_error(priceIndex(dat, pvar="prices",qvar="quantities", pervar = "time",
                          prodID = "prodID", indexMethod = "laspeyres",
                          output = "chained"),
               "The time period variable is not continuous. Missing periods: 3")
})

rm(dat)

load(system.file("testdata","testData_bilateral_quantity.RData",package = "IndexNumR"))

for(i in seq_along(indexMethods)){
  for(j in seq_along(outputTypes)){
    test_that("bilateral quantity index functions return the correct values",{
      expect_equal(quantityIndex(CES_sigma_2,pvar="prices",qvar="quantities",
                              pervar="time",prodID = "prodID",
                              indexMethod = indexMethods[i],
                              sample = "matched",output=outputTypes[j],
                              sigma=2),
                   as.matrix(testData[[paste0(indexMethods[i],"_",outputTypes[j])]]))
    })
  }
}

test_that("error is thrown when wrong column names are given",{
  expect_error(priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantitie",
                          pervar = "time",prodID = "prodID",indexMethod = "laspeyres",
                          sample="matched", output = "chained"),
               "are not column names of the input data frame")
})

rm(testData)
