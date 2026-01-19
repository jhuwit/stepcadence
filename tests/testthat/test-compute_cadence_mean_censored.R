test_that("compute_cadence_mean with counts applies the threshold", {

  set.seed(20250114)
  df = simulated_nhanes_steps %>%
    dplyr::mutate(counts = rpois(n = nrow(simulated_nhanes_steps),
                                 lambda = 500))
  res = compute_cadence_mean(df, group_cols = nhanes_group_cols())
  res0 = compute_cadence_mean(df, counts_threshold = 0, group_cols = nhanes_group_cols())

  testthat::expect_equal(
    res$mean_uncensored_cadence, res0$mean_uncensored_cadence
  )
  testthat::expect_error({
    testthat::expect_equal(
      res$mean_censored_cadence, res0$mean_censored_cadence
    )
  })

})
