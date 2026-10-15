test_that("compute_cadence_bouts finds bouts of the minimum length", {
  df = make_steps(c(0, rep(110, 10), 0, rep(120, 9), 0, rep(100, 12)))
  result = compute_cadence_bouts(df)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("n_bouts", "bout_minutes", "mean_bout_cadence"))
  expect_equal(result$n_bouts, 2L)
  expect_equal(result$bout_minutes, 22L)
  expect_equal(result$mean_bout_cadence, (1100 + 1200) / 22)
})

test_that("bout_tolerance bridges short interruptions only", {
  df = make_steps(c(0, rep(110, 6), 50, rep(110, 5), 0))
  expect_equal(compute_cadence_bouts(df)$n_bouts, 0L)
  result = compute_cadence_bouts(df, bout_tolerance = 1)
  expect_equal(result$n_bouts, 1L)
  expect_equal(result$bout_minutes, 12L)
  expect_equal(result$mean_bout_cadence, (11 * 110 + 50) / 12)

  # a 2-minute interruption is not bridged with tolerance 1
  df = make_steps(c(rep(110, 6), 50, 50, rep(110, 5)))
  expect_equal(compute_cadence_bouts(df, bout_tolerance = 1)$n_bouts, 0L)
  expect_equal(compute_cadence_bouts(df, bout_tolerance = 2)$n_bouts, 1L)

  # leading and trailing minutes below threshold are never part of a bout
  df = make_steps(c(50, rep(110, 10), 50))
  result = compute_cadence_bouts(df, bout_tolerance = 5)
  expect_equal(result$bout_minutes, 10L)
})

test_that("missing minutes are below threshold and excluded from the mean", {
  df = make_steps(c(rep(110, 5), NA, rep(110, 5)))
  expect_equal(compute_cadence_bouts(df)$n_bouts, 0L)
  result = compute_cadence_bouts(df, bout_tolerance = 1)
  expect_equal(result$bout_minutes, 11L)
  expect_equal(result$mean_bout_cadence, 110)
})

test_that("compute_cadence_bouts gives 0 bouts and NA mean with no bouts", {
  result = compute_cadence_bouts(make_steps(rep(0, 20)))
  expect_equal(result$n_bouts, 0L)
  expect_equal(result$bout_minutes, 0L)
  expect_identical(result$mean_bout_cadence, NA_real_)
})

test_that("compute_cadence_bouts groups and validates", {
  df = data.frame(id = rep(1:2, each = 10), time = rep(1:10, 2),
                  steps = c(rep(100, 10), rep(0, 10)))
  result = compute_cadence_bouts(df, group_cols = "id")
  expect_equal(result$n_bouts, c(1L, 0L))
  expect_error(compute_cadence_bouts(df, bout_threshold = 0),
               "bout_threshold")
  expect_error(compute_cadence_bouts(df, bout_min_length = 2.5),
               "bout_min_length")
  expect_error(compute_cadence_bouts(df, bout_tolerance = -1),
               "bout_tolerance")
})

test_that("compute_cadence_extended_bout follows Saint-Maurice et al.", {
  df = make_steps(c(0, 60, 70, 0, 80, 0, 100, 100, 100, 59))
  result = compute_cadence_extended_bout(df)
  expect_named(result, c("n_extended_bouts", "extended_bout_cadence"))
  expect_equal(result$n_extended_bouts, 2L)
  expect_equal(result$extended_bout_cadence, (60 + 70 + 300) / 5)
  expect_identical(
    compute_cadence_extended_bout(make_steps(c(0, 80, 0)))$extended_bout_cadence,
    NA_real_
  )
})
