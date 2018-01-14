context("geks indices")

load(system.file("testdata","testData_geks.RData",package = "IndexNumR"))

splices <- c("window","movement","mean")

for(j in seq_along(splices)){
  test_that("geks splice functions return the correct values",{
    expect_equal(GEKSIndex(CES_sigma_2,pvar="prices",qvar="quantities",
                            pervar="time",prodID = "prodID",
                            indexMethod = "tornqvist",
                            sample = "matched",window=11,
                           splice = splices[j]),
                 as.matrix(testData[[splices[j]]]))
  })
}

test_that("error is thrown when wrong column names are given",{
  expect_error(GEKSIndex(CES_sigma_2,pvar = "price",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "tornqvist",
                          sample="matched"),
               "are not column names of the input data frame")
})

rm(testData)
