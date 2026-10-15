test_that("compute_cadence_minutes_above counts cumulatively", {
  df = make_steps(c(0, 39.9, 40, 99, 100, 150, NA))
  result = compute_cadence_minutes_above(df)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("minutes_40plus", "minutes_100plus"))
  expect_equal(unlist(result), c(minutes_40plus = 4L, minutes_100plus = 2L))
})

test_that("compute_cadence_minutes_above sorts and names thresholds", {
  df = make_steps(c(0, 50, 125, 140))
  result = compute_cadence_minutes_above(df, thresholds = c(130, 60, 42.5))
  expect_named(result, c("minutes_42.5plus", "minutes_60plus",
                         "minutes_130plus"))
  expect_equal(unlist(result, use.names = FALSE), c(3L, 2L, 1L))
})

test_that("compute_cadence_minutes_above groups and validates", {
  df = data.frame(id = rep(1:2, each = 2), time = rep(1:2, 2),
                  steps = c(0, 120, 50, 50))
  result = compute_cadence_minutes_above(df, group_cols = "id")
  expect_equal(result$minutes_40plus, c(1L, 2L))
  expect_equal(result$minutes_100plus, c(1L, 0L))
  for (bad in list(0, -1, c(40, 40), NA_real_, Inf, "40", numeric(0))) {
    expect_error(compute_cadence_minutes_above(df, thresholds = bad),
                 "thresholds")
  }
})
