test_that("compute_cadence_mean computes uncensored and censored means", {
  df = make_steps(c(10, 20, 30, 40), counts = c(100, 600, 499, 500))
  result = compute_cadence_mean(df)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("n_minutes", "n_minutes_wear", "n_minutes_valid",
                         "mean_uncensored_cadence", "mean_censored_cadence"))
  expect_equal(result$n_minutes, 4L)
  expect_equal(result$n_minutes_valid, 4L)
  expect_true(is.na(result$n_minutes_wear))
  expect_equal(result$mean_uncensored_cadence, 25)
  # censored minutes count as 0 steps but stay in the denominator
  expect_equal(result$mean_censored_cadence, (20 + 40) / 4)

  result = compute_cadence_mean(df, counts_threshold = 0)
  expect_equal(result$mean_censored_cadence, result$mean_uncensored_cadence)
})

test_that("compute_cadence_mean censoring changes the mean", {
  df = simulated_nhanes_steps
  set.seed(20250114)
  df$counts = stats::rpois(nrow(df), lambda = 500)
  res = compute_cadence_mean(df, group_cols = nhanes_group_cols())
  res0 = compute_cadence_mean(df, counts_threshold = 0,
                              group_cols = nhanes_group_cols())
  expect_equal(res$mean_uncensored_cadence, res0$mean_uncensored_cadence)
  expect_true(all(res$mean_censored_cadence < res0$mean_censored_cadence))
})

test_that("compute_cadence_mean is NA without counts or steps", {
  result = compute_cadence_mean(make_steps(c(10, 20)))
  expect_equal(result$mean_uncensored_cadence, 15)
  expect_true(is.na(result$mean_censored_cadence))

  result = compute_cadence_mean(make_steps(c(NA_real_, NA_real_),
                                           counts = c(600, 600)))
  expect_equal(result$n_minutes_valid, 0L)
  expect_true(is.na(result$mean_uncensored_cadence))
  expect_true(is.na(result$mean_censored_cadence))
})

test_that("compute_cadence_mean drops minutes with missing counts", {
  df = make_steps(c(10, 20, 30, 40), counts = c(NA, 600, 100, 700))
  result = compute_cadence_mean(df)
  expect_equal(result$mean_uncensored_cadence, 25)
  expect_equal(result$mean_censored_cadence, (20 + 0 + 40) / 3)
})

test_that("compute_cadence_mean divides by wear time when masking", {
  df = make_steps(c(0, 10, 20, 30), wear = c(0, 1, 1, 1),
                  counts = c(0, 0, 600, 600))
  result = compute_cadence_mean(df)
  expect_equal(result$n_minutes_wear, 3L)
  expect_equal(result$mean_uncensored_cadence, 15)

  result = compute_cadence_mean(df, mask_wear = TRUE)
  expect_equal(result$n_minutes, 4L)
  expect_equal(result$n_minutes_wear, 3L)
  expect_equal(result$n_minutes_valid, 3L)
  expect_equal(result$mean_uncensored_cadence, 20)
  expect_equal(result$mean_censored_cadence, 50 / 3)
})

test_that("compute_cadence_mean has no grouping by default", {
  result = compute_cadence_mean(one_day())
  expect_equal(nrow(result), 1L)
  expect_equal(result$mean_uncensored_cadence, 15150 / 1440)
})

test_that("compute_cadence_mean validates counts", {
  expect_error(compute_cadence_mean(make_steps(1:2, counts = c(-1, 5))),
               "counts")
  expect_error(compute_cadence_mean(make_steps(1:2, counts = c("a", "b"))),
               "counts")
  for (bad in list(-1, NA, c(100, 500), "500", Inf)) {
    expect_error(compute_cadence_mean(make_steps(1:2),
                                      counts_threshold = bad),
                 "counts_threshold")
  }
  expect_error(compute_cadence_mean(make_steps(1:2, wear = c(1, 2))),
               "wear")
})
