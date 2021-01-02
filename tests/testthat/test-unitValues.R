context("unitValues functions")

test_that("unitValues function returns the correct values", {

  df <- CES_sigma_2
  # convert the monthly time variable into quarterly
  df$time <- ceiling(CES_sigma_2$time/3)

  # compute unit values using the quarterly time variable
  result <- unitValues(df, pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID")

  expected <- data.frame(
    prodID = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L),
    period = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L),
    quantities = c(1.6828282,10.3574383,5.7775985,
                   34.2941714,3.1488225,5.2115454,20.0514504,26.9706014,4.4087007,
                   10.3419066,14.2303051,31.8921328,6.401336,5.1532397,
                   29.2651552,28.6050935),
    unitValue = c(1.743535478,0.6622509274,0.9437975121,
                  0.5468516194,1.4453811036,1.1237581288,0.5376119109,
                  0.6975126784,1.2966820814,0.7996809619,0.7218112281,0.6817197566,
                  1.145162195,1.2769666057,0.5183794879,0.7662432846)
  )

  expect_equal(result, expected)

})

