test_that("input must be a data frame with time and steps", {
  expect_error(compute_cadence_bands(list(steps = 1:3)), "data frame")
  expect_error(compute_cadence_peak(list(time = 1:3, steps = 1:3)),
               "data frame")
  expect_error(compute_cadence_bands(data.frame(time = 1:3)),
               "Missing required columns: steps")
  expect_error(compute_cadence_peak(data.frame(steps = 1:3)),
               "Missing required columns: time")
  expect_error(compute_cadence_peak(one_day(), group_cols = "nope"),
               "Missing required columns: nope")
})

test_that("steps must be numeric, finite, and non-negative", {
  for (bad in list(c("1", "2"), c(1, -1), c(1, Inf))) {
    expect_error(compute_cadence_peak(make_steps(bad)), "Steps must be")
  }
  expect_no_error(compute_cadence_peak(make_steps(c(1, NA))))
})

test_that("time must be numeric or POSIXct, complete, and 1-minute", {
  df = make_steps(1:3)
  df$time = as.character(df$time)
  expect_error(compute_cadence_peak(df), "`time` must be numeric")

  df = make_steps(1:3)
  df$time[1] = NA
  expect_error(compute_cadence_peak(df), "must not contain NA")

  expect_error(compute_cadence_peak(make_steps(1:3)[c(2, 1, 3), ]),
               "sorted")
  expect_error(compute_cadence_peak(make_steps(1:5)[-3, ]),
               "equally spaced")

  df = make_steps(1:3)
  df$time = c(0, 2, 4)
  expect_error(compute_cadence_peak(df), "1-minute")

  df$time = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 30, 60)
  expect_error(compute_cadence_peak(df), "1-minute")
})

test_that("forgetting group_cols on multi-day data gives a helpful error", {
  expect_error(compute_cadence_peak(simulated_nhanes_steps), "group_cols")
})

test_that("group_cols must be a character vector", {
  expect_error(compute_cadence_peak(one_day(), group_cols = 1),
               "group_cols")
})

test_that("validate_steps_data warns if checking minutes without time", {
  expect_warning(
    validate_steps_data(data.frame(steps = 1), columns = "steps",
                        check_minute = TRUE),
    "check_minute"
  )
})

test_that("mask_wear needs a valid wear column", {
  expect_error(compute_cadence_peak(make_steps(1:3), mask_wear = TRUE),
               "requires a `wear` column")
  expect_error(
    compute_cadence_peak(make_steps(1:3, wear = c(1, 2, 1)), mask_wear = TRUE),
    "`wear` must be logical"
  )
  expect_error(compute_cadence_peak(make_steps(1:3), mask_wear = "yes"),
               "mask_wear")

  logical_wear = make_steps(1:3, wear = c(TRUE, NA, TRUE))
  numeric_wear = make_steps(1:3, wear = c(1, NA, 1))
  expect_equal(compute_cadence_mean(logical_wear, mask_wear = TRUE),
               compute_cadence_mean(numeric_wear, mask_wear = TRUE))
  expect_equal(
    compute_cadence_mean(logical_wear, mask_wear = TRUE)$n_minutes_valid, 2L
  )
})

test_that("nhanes_group_cols returns the NHANES participant-day columns", {
  expect_equal(nhanes_group_cols(), c("SEQN", "PAXDAYM"))
})
