
test_that("dominicksData fails nicely with bad file names", {
  expect_error(dominicksData("Beer", movementFile = "badfilename"))
  expect_error(dominicksData("Beer", UPCFile = "badfilename"))
})

test_that("dominicksData fails nicely with bad category names",{
  expect_error(dominicksData("badCategory"))
  expect_error(dominicksData("Refrigerated Juices"))
})

test_that("getDominicksFilename gets the right filename for different categories", {

  expect_equal(getDominicksFileName("beer", "upc"), "upcber.csv")
  expect_equal(getDominicksFileName("beer", "movement"), "wber.csv")

  expect_equal(getDominicksFileName("crackers", "upc"), "upccra.csv")
  expect_equal(getDominicksFileName("crackers", "movement"), "wcra.csv")

})

test_that("dominicks data cleaning code works", {

  dfmove <- data.frame(OK = c(rep(1, 4), 0),
                   PRICE = c(0, 1:4),
                   QTY = 1:5,
                   MOVE = 1:5,
                   UPC = c(rep(1, 3), rep(2, 2)),
                   WEEK = rep(1, 5))
  dfupc <- data.frame(UPC = 1:2,
                      DESCRIPTION = c("item1", "item2"))

  merged <- cleanAndMergeDominicks(dfmove, dfupc)
  expected <- structure(list(week = c(1, 1, 1),
                             upc = c(1, 1, 2),
                             price = c(1, 2, 3),
                             quantity = c(1, 1, 1),
                             expenditure = c(1, 2, 3),
                             description = c("item1", "item1", "item2"),
                             start = structure(c(7196, 7196, 7196), class = "Date"),
                             end = structure(c(7202, 7202, 7202), class = "Date"),
                             specialEvents = c("", "", "")),
                        row.names = c(NA, -3L),
                        class = "data.frame")

  expect_equal(merged, expected)

})
