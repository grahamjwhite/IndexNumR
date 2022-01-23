context("geks indices")

load(system.file("testdata","testData_geks.RData",package = "IndexNumR"))

test_that("geks splice functions return the correct values",{

  splices <- c("window", "movement", "mean", "half", "wisp", "hasp", "mean_pub")

  for(j in seq_along(splices)){
    expect_equal(GEKSIndex(CES_sigma_2,
                           pvar="prices",
                           qvar="quantities",
                           pervar="time",
                           prodID = "prodID",
                           indexMethod = "tornqvist",
                           sample = "matched",
                           window=11,
                           splice = splices[j]),
                 as.matrix(testData[[splices[j]]]))

  }
})

test_that("geks functions return the correct values for different index methods", {

  methods <- c("fisher", "tornqvist", "tpd", "walsh", "jevons")

  for(j in seq_along(methods)){
    expect_equal(GEKSIndex(CES_sigma_2,
                           pvar="prices",
                           qvar="quantities",
                           pervar="time",
                           prodID = "prodID",
                           indexMethod = methods[j],
                           sample = "matched",
                           window = 12),
                 as.matrix(testData[[methods[j]]]))

  }
})

test_that("error is thrown when wrong column names are given",{
  expect_error(GEKSIndex(CES_sigma_2,pvar = "price",qvar = "quantities",
                          pervar = "time",prodID = "prodID",indexMethod = "tornqvist",
                          sample="matched"),
               "are not column names of the input data frame")
})

rm(testData)

#load CES_sigma_2 and make period 3 missing
dat <- CES_sigma_2
dat$time[dat$time==3] = 2

test_that("error is thrown when a time period is missing",{
  expect_error(GEKSIndex(dat, pvar="prices",qvar="quantities", pervar = "time",
                          prodID = "prodID", indexMethod = "tornqvist",
                          window = 12),
               "The time period variable is not continuous. Missing periods: 3")
})

rm(dat)

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

test_that("GEKS index returns the same answer regardless of product ordering",{

  indexMethods <- c("fisher", "tornqvist")

  doIndex <- function(dataset, method){
    GEKSIndex(dataset, pvar = "prices", qvar = "quantities", prodID = "prodID",
               pervar = "time", indexMethod = method, window = 3, splice = "mean")
  }

  indexEqual <- function(dataset1, method, dataset2){
    expect_equal(doIndex(dataset1, method), doIndex(dataset2, method))
  }

  for(i in seq_along(indexMethods)){
    indexEqual(CESOrdered, indexMethods[i], CESUnordered)
  }

})

test_that("intGEKS gives the same result as GEKS with no product change", {

  indexMethods <- c("fisher", "tornqvist", "tpd", "walsh", "jevons")

  testMethod <- function(method){
    intFalse <- GEKSIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                          prodID = "prodID", indexMethod = method, window = 12, intGEKS = FALSE)

    intTrue <- GEKSIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
                         prodID = "prodID", indexMethod = method, window = 12, intGEKS = TRUE)

    expect_equal(intFalse, intTrue)
  }

  for(i in 1:length(indexMethods)){
    testMethod(indexMethods[i])
  }

})


test_that("can replicate intGEKS result in Lamboray & Krsinich", {

  x1 <- c(1, 1*cumprod(rep(0.99, 7)))
  x2 <- c(1, 1*(0.9), 1*(0.9^2), 1*(0.9^3), rep(NA, 4))
  x3 <- c(rep(NA, 4), 1, 1*(0.9), 1*(0.9^2), 1*(0.9^3))

  df <- data.frame(p = c(x1, x2, x3), q = rep(1, 24), prod = c(rep(1, 8), rep(2, 8), rep(3, 8)), t = rep(1:8, 3))
  df <- df[!is.na(df$p),]

  ind <- IndexNumR::GEKSIndex(df, pvar = "p", qvar = "q", pervar = "t", prodID = "prod",
                              window = 8, sample = "matched", indexMethod = "tornqvist", intGEKS = TRUE)

  # test for the result in table 2 in Lamboray and Krsinich (2016)
  expect_equal(ind[5]/ind[4], 0.99)

})


test_that("The right GEKSIndex result is given when prices are imputed", {

  df <- CES_sigma_2[-c(2:3),]
  result <- GEKSIndex(df, pvar = "prices", qvar = "quantities", pervar = "time",
                    prodID = "prodID", indexMethod = "fisher", imputePrices = "carry", window = 12)

  expect_equal(result, as.matrix(c(1, 0.876105290522952, 1.08127159852239, 1.10437571515941,
                                   0.911053419297449, 1.16689616688954, 1.1114839943134, 0.922249166063837,
                                   1.08436534352444, 0.930263143473634, 1.11519231402879, 1.11806626998895
  )))

})
