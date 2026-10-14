context("input types")

# remove some observations so that imputation and matching affect the results
typesData <- CES_sigma_2[-c(1, 2, 14, 15, 30),]

# the same data with the columns stored as other types
typeVariants <- list(
  "character periods" = transform(typesData, time = as.character(time)),
  "factor periods" = transform(typesData, time = factor(as.character(time))),
  "character prices" = transform(typesData, prices = as.character(prices)),
  "factor prices" = transform(typesData, prices = factor(prices)),
  "character quantities" = transform(typesData, quantities = as.character(quantities)),
  "factor quantities" = transform(typesData, quantities = factor(quantities)),
  "factor product IDs with unused levels" = transform(typesData, prodID = factor(prodID, levels = 1:6))
)

typeFunctions <- list(
  priceIndex = function(x) priceIndex(x, pvar = "prices", qvar = "quantities", pervar = "time",
                                      prodID = "prodID", indexMethod = "tornqvist",
                                      imputePrices = "carry"),
  quantityIndex = function(x) quantityIndex(x, pvar = "prices", qvar = "quantities", pervar = "time",
                                            prodID = "prodID", indexMethod = "fisher"),
  GEKSIndex = function(x) GEKSIndex(x, "prices", "quantities", "time", "tornqvist",
                                    prodID = "prodID", window = 12, imputePrices = "carry"),
  GKIndex = function(x) GKIndex(x, "prices", "quantities", "time", "prodID", window = 12),
  WTPDIndex = function(x) WTPDIndex(x, "prices", "quantities", "time", "prodID", window = 12),
  priceIndicator = function(x) priceIndicator(x, "prices", "quantities", "time", "prodID", "laspeyres"),
  valueDecomposition = function(x) valueDecomposition(x, "prices", "quantities", "time", "prodID", "bennet"),
  values = function(x) values(x, "prices", "quantities", "time", "prodID"),
  unitValues = function(x) unitValues(x, "prices", "quantities", "time", "prodID"),
  imputeCarryPrices = function(x) imputeCarryPrices(x, "prices", "quantities", "time", "prodID"),
  imputeQuantities = function(x) imputeQuantities(x, "prices", "time", "prodID"),
  shares = function(x) shares(x, "prices", "quantities", "time", "prodID"),
  predictedShares = function(x) predictedShares(x, "prices", "quantities", "time", "prodID"),
  evaluateMatched = function(x) evaluateMatched(x, "prices", "quantities", "time", "prodID"),
  productChanges = function(x) productChanges(x, "time", "prodID"),
  relativeDissimilarity = function(x) relativeDissimilarity(x, "prices", "quantities", "time", "prodID"),
  elasticity = function(x) elasticity(x, "prices", "quantities", "time", "prodID", compIndex = "ces"),
  groupIndexes = function(x) groupIndexes("group", "priceIndex",
                                          list(x = transform(x, group = as.numeric(as.character(prodID)) %% 2),
                                               pvar = "prices", qvar = "quantities", pervar = "time",
                                               prodID = "prodID", indexMethod = "fisher"))
)

# the numbers in a result. Product IDs are compared as numbers, since a factor
# product ID is returned as character.
resultNumbers <- function(result){
  if(is.data.frame(result)) result <- as.list(result)
  if(!is.list(result)) result <- list(result)
  unname(unlist(rapply(result, function(z) as.numeric(as.character(z)), how = "list")))
}

test_that("functions give the same results when columns are stored as other types", {

  for(f in names(typeFunctions)){
    expected <- resultNumbers(typeFunctions[[f]](typesData))
    for(v in names(typeVariants)){
      expect_equal(resultNumbers(typeFunctions[[f]](typeVariants[[v]])), expected,
                   info = paste(f, "with", v))
    }
  }

})


test_that("functions accept a data.table", {

  skip_if_not_installed("data.table")

  dt <- data.table::as.data.table(transform(typesData, prodID = paste0("p", prodID)))
  df <- as.data.frame(dt)

  for(f in names(typeFunctions)){
    if(f == "groupIndexes") next
    expect_equal(typeFunctions[[f]](dt), typeFunctions[[f]](df), info = f)
  }

})


test_that("factor time periods do not hide gaps or a late start", {

  lateStart <- CES_sigma_2[CES_sigma_2$time >= 3,]
  lateStart$time <- factor(lateStart$time)
  expect_error(GEKSIndex(lateStart, "prices", "quantities", "time", "tornqvist",
                         prodID = "prodID", window = 10),
               "must start at 1")

  gap <- CES_sigma_2[CES_sigma_2$time != 6,]
  gap$time <- factor(gap$time)
  expect_error(GEKSIndex(gap, "prices", "quantities", "time", "tornqvist",
                         prodID = "prodID", window = 11),
               "not continuous")

})
