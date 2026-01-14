test_that("validate_steps_data cases are bad", {

  # no mask in the data
  compute_cadence_max(simulated_nhanes_steps, mask_wear = TRUE)

  # not a data.frame
  testthat::expect_error({
    compute_cadence_bands(list(steps = simulated_nhanes_steps$steps))
  }, regexp = "data frame")

  # no steps in the data
  df = simulated_nhanes_steps %>%
    dplyr::select(-steps)
  testthat::expect_error({
    compute_cadence_bands(df)
  }, "all of these")

  # time is missing in validate
  df = simulated_nhanes_steps %>%
    dplyr::select(-time)
  testthat::expect_error({
    compute_cadence_peak(df)
  }, "required column")

  # steps has NA values
  df = simulated_nhanes_steps %>%
    dplyr::mutate(time = ifelse(dplyr::row_number() == 1, NA, time))
  testthat::expect_error({
    compute_cadence_peak(df)
  }, "must not contain NA")

  # no group_cols (data is mixed by ID)
  testthat::expect_error({
    compute_cadence_peak(simulated_nhanes_steps, group_cols = NULL)
  }, "non-decreasing")

  # trying POSIXt
  df = simulated_nhanes_steps %>%
    as.data.frame() %>%
    dplyr::mutate(time = (PAXDAYM-1) * 1440 + time,
                  time = lubridate::as_date("2000-01-01") + lubridate::as.period(time, unit = "minutes")
    )
  # this may not work yet - need to fix validation
  # compute_cadence_peak(df)


})
