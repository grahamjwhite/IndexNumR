
test_that("dominicksData fails nicely with bad file names", {
  expect_error(dominicksData("Beer", movementFile = "badfilename"))
  expect_error(dominicksData("Beer", UPCFile = "badfilename"))
})

test_that("dominicksData fails nicely with bad category names",{
  expect_error(dominicksData("badCategory"))
  expect_error(dominicksData("Refrigerated Juices"))
})
