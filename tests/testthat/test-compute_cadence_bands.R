test_that("compute_cadence_bands works with grouping variables", {

  test_result <- compute_cadence_bands(simulated_nhanes_steps %>%
                                         dplyr::group_by(PAXDAYM))
  testthat::expect_true(
    dplyr::is.grouped_df(test_result)
  )
})
