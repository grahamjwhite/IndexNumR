context("bilateral indices")

load(system.file("testdata","testData_bilateral.RData",package = "IndexNumR"))

indexMethods <- c("laspeyres","paasche","fisher","tornqvist","satovartia",
                  "dutot","carli","jevons","harmonic","cswd","walsh")
outputTypes <- c("pop","chained","fixedbase")

for(i in 1:length(indexMethods)){
  for(j in 1:length(outputTypes)){
    test_that("bilateral price index functions return the correct values",{
      expect_equal(priceIndex(CES_sigma_2,pvar="prices",qvar="quantities",
                              pervar="time",prodID = "prodID",
                              indexMethod = indexMethods[i],
                              sample = "matched",output=outputTypes[j]),
                   as.matrix(testData[[paste0(indexMethods[i],"_",outputTypes[j])]]))
    })
  }
}

rm(testData)


load(system.file("testdata","testData_bilateral_quantity.RData",package = "IndexNumR"))

for(i in 1:length(indexMethods)){
  for(j in 1:length(outputTypes)){
    test_that("bilateral quantity index functions return the correct values",{
      expect_equal(quantityIndex(CES_sigma_2,pvar="prices",qvar="quantities",
                              pervar="time",prodID = "prodID",
                              indexMethod = indexMethods[i],
                              sample = "matched",output=outputTypes[j]),
                   as.matrix(testData[[paste0(indexMethods[i],"_",outputTypes[j])]]))
    })
  }
}

rm(testData)
