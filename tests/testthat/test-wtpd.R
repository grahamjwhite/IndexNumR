
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
                           0.939283785493012, 1.22946886276831, 1.16928578010074, 0.962925047843644,
                           1.13966638509599, 0.949589626761819, 1.16793031098926, 1.17307644429241)))

  # wisp
  expect_equal(WTPDIndex(CES_sigma_2,
                         pvar = "prices",
                         qvar = "quantities",
                         prodID = "prodID",
                         pervar = "time",
                         window = 11,
                         splice = "wisp"),
               as.matrix(c(1, 0.88125316126336, 1.11113003298188, 1.15577105831891,
                           0.939283785493012, 1.22946886276831, 1.16928578010074, 0.962925047843644,
                           1.13966638509599, 0.949589626761819, 1.16793031098926, 1.18221020311053)
               ))

  # hasp
  expect_equal(WTPDIndex(CES_sigma_2,
                         pvar = "prices",
                         qvar = "quantities",
                         prodID = "prodID",
                         pervar = "time",
                         window = 11,
                         splice = "hasp"),
               as.matrix(c(1, 0.88125316126336, 1.11113003298188, 1.15577105831891,
                           0.939283785493012, 1.22946886276831, 1.16928578010074, 0.962925047843644,
                           1.13966638509599, 0.949589626761819, 1.16793031098926, 1.17307644429241)))

  # mean_pub
  expect_equal(WTPDIndex(CES_sigma_2,
                         pvar = "prices",
                         qvar = "quantities",
                         prodID = "prodID",
                         pervar = "time",
                         window = 11,
                         splice = "mean_pub"),
               as.matrix(c(1, 0.88125316126336, 1.11113003298188, 1.15577105831891,
                           0.939283785493012, 1.22946886276831, 1.16928578010074, 0.962925047843644,
                           1.13966638509599, 0.949589626761819, 1.16793031098926, 1.17283932343105)
               ))

})

# show that the method used by IndexNumR is the same as a pooled WLS regression when observations are missing
test_that("IndexNumR equals a WLS regression on pooled data", {

  # remove a product from some periods
  df <- CES_sigma_2[-(45:48),]

  # set up the regression data
  df <- df[order(df$time, df$prodID),]
  df$logP <- log(df$prices)
  df$e <- df$prices * df$quantities
  e <- stats::aggregate(e ~ time, data = df, FUN = sum)
  colnames(e) <- c("time", "totalExp")
  df <- merge(df, e, by = "time")
  df$s <- df$e/df$totalExp

  # run the WLS regression and extract time coefficients
  reg <- stats::lm(logP ~ as.factor(time) + as.factor(prodID), data = df, weights = s)
  b <- as.matrix(c(1, exp(coef(reg)[grep("time", names(coef(reg)))])))
  attributes(b) <- list(dim = c(12,1))

  # compute with IndexNumR
  i <- WTPDIndex(CES_sigma_2[-(45:48),], pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID", window = 12)

  expect_equal(b, i)

})


test_that("The right WTPDIndex result is given when prices are imputed", {

  df <- CES_sigma_2[-c(2:3),]
  result <- WTPDIndex(df, pvar = "prices", qvar = "quantities", pervar = "time",
                      prodID = "prodID", imputePrices = "carry", window = 12)

  expect_equal(result, as.matrix(c(1, 0.84302144386335, 1.09076856212972, 1.16065329874175,
                                   0.944527046394152, 1.23691866972485, 1.17634227942966, 0.966190720355013,
                                   1.14752185188379, 0.957115889683172, 1.17791153172507, 1.1778361047407
  )))

})


test_that("Error is thrown for wrong splice method", {

  expect_error(WTPDIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",
                         pervar = "time", prodID = "prodID",
                         sample = "matched", window = 12, splice = "wrongSplice"),
               "Not a valid splicing method.")

})

test_that("Error is thrown for wrong imputePrices parameter", {

  expect_error(WTPDIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",
                         pervar = "time", prodID = "prodID",
                         sample = "matched", window = 12, splice = "window", imputePrices = "wrongImpute"),
               "Invalid imputePrices argument")

})

