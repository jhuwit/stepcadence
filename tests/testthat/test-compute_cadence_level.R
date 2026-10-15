test_that("compute_cadence_level boundaries use [0, t1), [t1, t2), [t2, Inf)", {
  df = make_steps(c(0, 59, 59.9, 60, 119, 120, 300))
  result = compute_cadence_level(df)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("low_cadence", "moderate_cadence", "high_cadence"))
  expect_equal(unlist(result), c(low_cadence = 3L, moderate_cadence = 2L,
                                 high_cadence = 2L))

  result = compute_cadence_level(df, step_thresholds = c(30, 100))
  expect_equal(unlist(result), c(low_cadence = 1L, moderate_cadence = 3L,
                                 high_cadence = 3L))
})

test_that("compute_cadence_level sorts thresholds", {
  df = make_steps(c(10, 70, 130, 200))
  expect_equal(compute_cadence_level(df, step_thresholds = c(120, 60)),
               compute_cadence_level(df, step_thresholds = c(60, 120)))
})

test_that("compute_cadence_level rejects bad thresholds", {
  df = make_steps(1:5)
  for (bad in list(60, c(60, 60), c(0, 60), c(-1, 60), c(60, NA),
                   c(60, Inf), c("60", "120"), c(30, 60, 90))) {
    expect_error(compute_cadence_level(df, step_thresholds = bad),
                 "step_thresholds")
  }
})

test_that("compute_cadence_level does not count NA steps", {
  result = compute_cadence_level(make_steps(c(0, NA, 70, NA)))
  expect_named(result, c("low_cadence", "moderate_cadence", "high_cadence"))
  expect_equal(unlist(result), c(low_cadence = 1L, moderate_cadence = 1L,
                                 high_cadence = 0L))
})

test_that("compute_cadence_level groups by group_cols or dplyr groups", {
  df = data.frame(id = rep(1:2, each = 3), time = rep(1:3, 2),
                  steps = c(0, 70, 130, 0, 0, 0))
  result = compute_cadence_level(df, group_cols = "id")
  expect_equal(result$id, 1:2)
  expect_equal(result$low_cadence, c(1L, 3L))
  expect_equal(result$high_cadence, c(1L, 0L))
  expect_equal(compute_cadence_level(dplyr::group_by(df, id)), result)
})
