
test_that("GKIndex produces the right answer for CES_sigma_2", {

  expect_equal(GKIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 12),
               as.matrix(c(1, 0.87763982, 1.14963977, 1.20948388,
                           0.97735333, 1.30228207, 1.23643987,
                           1.00131376, 1.20675381, 0.98419301,
                           1.23476104, 1.23245315)))

})

test_that("GKIndex produces the right answer for different splices", {

  # mean
  expect_equal(GKIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 11,
                       splice = "mean"),
               as.matrix(c(1, 0.880516797, 1.146776165, 1.204788832,
                           0.970884329, 1.294436941, 1.228859847,
                           1.000095564, 1.198013216, 0.975391622,
                           1.223185405, 1.230446268)))

  # movement
  expect_equal(GKIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 11,
                       splice = "movement"),
               as.matrix(c(1, 0.880516797, 1.146776165, 1.204788832,
                           0.970884329, 1.294436941, 1.228859847,
                           1.000095564, 1.198013216, 0.975391622,
                           1.223185405, 1.223542862)))

  # window
  expect_equal(GKIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 11,
                       splice = "window"),
               as.matrix(c(1, 0.880516797, 1.146776165, 1.204788832,
                           0.970884329, 1.294436941, 1.228859847,
                           1.000095564, 1.198013216, 0.975391622,
                           1.223185405, 1.243101)))

  # half
  expect_equal(GKIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 11,
                       splice = "half"),
               as.matrix(c(1, 0.880516792639476, 1.14677616370861, 1.20478883103346,
                           0.970884329042465, 1.29443693892348, 1.22885984521462, 1.00009556334273,
                           1.19801321479787, 0.975391622972356, 1.22318540412509, 1.23085666351138)))

  # wisp
  expect_equal(GKIndex(CES_sigma_2,
                         pvar = "prices",
                         qvar = "quantities",
                         prodID = "prodID",
                         pervar = "time",
                         window = 11,
                         splice = "wisp"),
               as.matrix(c(1, 0.880516792639476, 1.14677616370861, 1.20478883103346,
                           0.970884329042465, 1.29443693892348, 1.22885984521462, 1.00009556334273,
                           1.19801321479787, 0.975391622972356, 1.22318540412509, 1.24310097571958)))

  # hasp
  expect_equal(GKIndex(CES_sigma_2,
                         pvar = "prices",
                         qvar = "quantities",
                         prodID = "prodID",
                         pervar = "time",
                         window = 11,
                         splice = "hasp"),
               as.matrix(c(1, 0.880516792639476, 1.14677616370861, 1.20478883103346,
                           0.970884329042465, 1.29443693892348, 1.22885984521462, 1.00009556334273,
                           1.19801321479787, 0.975391622972356, 1.22318540412509, 1.23085666351138)))

  # mean_pub
  expect_equal(GKIndex(CES_sigma_2,
                         pvar = "prices",
                         qvar = "quantities",
                         prodID = "prodID",
                         pervar = "time",
                         window = 11,
                         splice = "mean_pub"),
               as.matrix(c(1, 0.880516792639476, 1.14677616370861, 1.20478883103346,
                           0.970884329042465, 1.29443693892348, 1.22885984521462, 1.00009556334273,
                           1.19801321479787, 0.975391622972356, 1.22318540412509, 1.23044626729226)))

})
