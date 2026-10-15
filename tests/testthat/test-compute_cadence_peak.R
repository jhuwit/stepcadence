test_that("compute_cadence_peak uses the highest non-consecutive minutes", {
  # 100 at the start and end, so the top 30 minutes are not consecutive
  steps = c(rep(100, 15), rep(0, 50), rep(100, 15), rep(10, 30), 150)
  result = compute_cadence_peak(make_steps(steps))
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("peak_1min", "peak_30min", "peak_60min"))
  expect_equal(result$peak_1min, 150)
  expect_equal(result$peak_30min, (150 + 29 * 100) / 30)
  expect_equal(result$peak_60min, (150 + 30 * 100 + 29 * 10) / 60)
})

test_that("compute_cadence_peak is NA without enough minutes", {
  result = compute_cadence_peak(make_steps(c(100, rep(0, 39))))
  expect_equal(result$peak_1min, 100)
  expect_equal(result$peak_30min, 10 / 3)
  expect_true(is.na(result$peak_60min))

  # NA minutes do not count toward the window
  steps = c(rep(50, 29), rep(NA, 40))
  result = compute_cadence_peak(make_steps(steps))
  expect_equal(result$peak_1min, 50)
  expect_true(is.na(result$peak_30min))

  expect_no_warning(
    result <- compute_cadence_peak(make_steps(rep(NA_real_, 70)))
  )
  expect_true(all(is.na(unlist(result))))
})

test_that("compute_cadence_peak masks non-wear", {
  df = make_steps(c(200, rep(10, 59)), wear = c(FALSE, rep(TRUE, 59)))
  expect_equal(compute_cadence_peak(df)$peak_1min, 200)
  expect_equal(compute_cadence_peak(df, mask_wear = TRUE)$peak_1min, 10)

  df$wear = as.integer(df$wear)
  expect_equal(compute_cadence_peak(df, mask_wear = TRUE)$peak_1min, 10)

  # a day with no wear has no peaks
  df$wear = 0L
  expect_no_warning(result <- compute_cadence_peak(df, mask_wear = TRUE))
  expect_true(all(is.na(unlist(result))))
})

test_that("compute_cadence_peak groups by group_cols or dplyr groups", {
  result = compute_cadence_peak(simulated_nhanes_steps,
                                group_cols = nhanes_group_cols())
  expect_equal(nrow(result), 6L)
  expect_named(result, c("SEQN", "PAXDAYM", "peak_1min", "peak_30min",
                         "peak_60min"))
  expect_equal(result$peak_60min, rep(165, 6))
  expect_equal(
    compute_cadence_peak(dplyr::group_by(simulated_nhanes_steps,
                                         SEQN, PAXDAYM)),
    result
  )
})

test_that("compute_cadence_peak accepts POSIXct times", {
  df = one_day()
  df$time = as.POSIXct("2020-01-01", tz = "UTC") + (df$time - 1) * 60
  expect_equal(compute_cadence_peak(df), compute_cadence_peak(one_day()))

  df = simulated_nhanes_steps
  df$time = as.POSIXct("2020-01-01", tz = "UTC") +
    (df$PAXDAYM - 1) * 86400 + (df$time - 1) * 60
  expect_equal(nrow(compute_cadence_peak(df, group_cols = "SEQN")), 3L)
})
