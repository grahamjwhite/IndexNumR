
test_that("WTPDIndex returns the right numbers", {

  expect_equal(WTPDIndex(CES_sigma_2, "prices", "quantities", "time", "prodID", window = 12),
               as.matrix(c(1, 0.880339868502394, 1.11310561070234, 1.15879006335178,
                           0.944228876424154, 1.23413422819567, 1.17381864977063,
                           0.96466253410287, 1.14473361053433, 0.955804858341343,
                           1.17418844896016, 1.17377193225844)

               ))
})

test_that("WTPDIndex produces the right answer for different splices", {

  # mean
  expect_equal(WTPDIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 11,
                       splice = "mean"),
               as.matrix(c(1, 0.88125316126336, 1.11113003298188, 1.15577105831891,
                           0.939283785493012, 1.22946886276831, 1.16928578010074,
                           0.962925047843644, 1.13966638509599, 0.949589626761819,
                           1.16793031098926, 1.17283932343105)

               ))

  # movement
  expect_equal(WTPDIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 11,
                       splice = "movement"),
               as.matrix(c(1, 0.88125316126336, 1.11113003298188, 1.15577105831891,
               0.939283785493012, 1.22946886276831, 1.16928578010074,
               0.962925047843644, 1.13966638509599, 0.949589626761819,
               1.16793031098926, 1.169222231299)

               ))

  # window
  expect_equal(WTPDIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 11,
                       splice = "window"),
               as.matrix(c(1, 0.88125316126336, 1.11113003298188, 1.15577105831891,
                           0.939283785493012, 1.22946886276831, 1.16928578010074,
                           0.962925047843644, 1.13966638509599, 0.949589626761819,
                           1.16793031098926, 1.18221020311053)

               ))

  # half
  expect_equal(WTPDIndex(CES_sigma_2,
                       pvar = "prices",
                       qvar = "quantities",
                       prodID = "prodID",
                       pervar = "time",
                       window = 11,
                       splice = "half"),
               as.matrix(c(1, 0.88125316126336, 1.11113003298188, 1.15577105831891,
                           0.939283785493012, 1.22946886276831, 1.16928578010074,
                           0.962925047843644, 1.13966638509599, 0.949589626761819,
                           1.16793031098926, 1.17283457648912)

               ))

})
