test_that("compute_cadence_max uses consecutive minutes", {
  # top minutes are split, so the best 30 consecutive is lower than peak 30
  steps = c(rep(100, 15), rep(0, 50), rep(100, 15), rep(0, 40))
  result = compute_cadence_max(make_steps(steps))
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("max_5min", "max_20min", "max_30min", "max_60min"))
  expect_equal(result$max_5min, 100)
  expect_equal(result$max_20min, 75)
  expect_equal(result$max_30min, 50)
  expect_equal(result$max_60min, 25)
  expect_equal(compute_cadence_peak(make_steps(steps))$peak_30min, 100)
})

test_that("compute_cadence_max is NA when no full window exists", {
  result = compute_cadence_max(make_steps(rep(100, 10)))
  expect_equal(result$max_5min, 100)
  expect_true(is.na(result$max_20min))
  expect_true(is.na(result$max_30min))
  expect_true(is.na(result$max_60min))

  expect_no_warning(
    result <- compute_cadence_max(make_steps(rep(NA_real_, 70)))
  )
  expect_true(all(is.na(unlist(result))))
})

test_that("compute_cadence_max skips windows with masked minutes", {
  wear = rep(TRUE, 40)
  wear[15] = FALSE
  df = make_steps(c(rep(100, 30), rep(0, 10)), wear = wear)

  result = compute_cadence_max(df)
  expect_equal(result$max_30min, 100)

  result = compute_cadence_max(df, mask_wear = TRUE)
  expect_equal(result$max_5min, 100)
  # best 20-minute window without minute 15 is minutes 16-35
  expect_equal(result$max_20min, 75)
  expect_true(is.na(result$max_30min))
})

test_that("compute_cadence_max matches the simulated data", {
  result = compute_cadence_max(simulated_nhanes_steps,
                               group_cols = nhanes_group_cols())
  expect_equal(nrow(result), 6L)
  expect_equal(result$max_30min, rep(200, 6))
  expect_equal(result$max_60min, rep(100, 6))
})
