test_that("compute_daily_steps sums steps and stepping minutes", {
  result = compute_daily_steps(make_steps(c(0, 10, 0, 100, NA, 0.5)))
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("total_steps", "n_minutes_stepping",
                         "mean_stepping_cadence"))
  expect_equal(result$total_steps, 110.5)
  expect_equal(result$n_minutes_stepping, 3L)
  expect_equal(result$mean_stepping_cadence, 110.5 / 3)
})

test_that("compute_daily_steps gives NA when there are no steps", {
  result = compute_daily_steps(make_steps(c(NA_real_, NA_real_)))
  expect_identical(result$total_steps, NA_real_)
  expect_equal(result$n_minutes_stepping, 0L)
  expect_identical(result$mean_stepping_cadence, NA_real_)

  result = compute_daily_steps(make_steps(c(0, 0)))
  expect_equal(result$total_steps, 0)
  expect_identical(result$mean_stepping_cadence, NA_real_)
})

test_that("compute_daily_steps masks non-wear and groups", {
  df = make_steps(c(10, 20, 30, 40), wear = c(1, 0, 1, 1), id = c(1, 1, 2, 2))
  result = compute_daily_steps(df, mask_wear = TRUE, group_cols = "id")
  expect_equal(result$total_steps, c(10, 70))
  expect_equal(result$n_minutes_stepping, c(1L, 2L))
})

test_that("step_volume_category uses Tudor-Locke & Bassett cut points", {
  result = step_volume_category(c(0, 4999, 5000, 7499, 7500, 9999, 10000,
                                  12499, 12500, 30000, NA))
  expect_s3_class(result, "factor")
  expect_equal(levels(result), c("sedentary", "low_active",
                                 "somewhat_active", "active",
                                 "highly_active"))
  expect_equal(as.character(result),
               c(rep(c("sedentary", "low_active", "somewhat_active",
                       "active", "highly_active"), each = 2), NA))
  expect_error(step_volume_category(-1), "steps")
  expect_error(step_volume_category("1000"), "steps")
})
