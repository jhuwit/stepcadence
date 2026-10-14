all_bands = c("zero", "incidental", "sporadic", "purposeful", "slow_walk",
              "medium_walk", "brisk_walk", "faster_ambulation")

test_that("cadence band boundaries follow Tudor-Locke et al. (2018)", {
  df = data.frame(
    steps = c(0, 0.5, 1, 19, 19.5, 20, 39, 40, 59, 60, 79, 80, 99, 100,
              119, 120, 200)
  )
  result = compute_cadence_bands(df)
  expect_s3_class(result$cadence_band, "factor")
  expect_equal(levels(result$cadence_band), all_bands)
  expect_equal(
    as.character(result$cadence_band),
    c("zero", "incidental", "incidental", "incidental", "incidental",
      "sporadic", "sporadic", "purposeful", "purposeful",
      "slow_walk", "slow_walk", "medium_walk", "medium_walk",
      "brisk_walk", "brisk_walk", "faster_ambulation", "faster_ambulation")
  )
})

test_that("compute_cadence_bands keeps NA steps as NA bands", {
  result = compute_cadence_bands(data.frame(steps = c(0, NA, 50)))
  expect_equal(as.character(result$cadence_band), c("zero", NA, "purposeful"))
})

test_that("compute_cadence_bands keeps dplyr groups and does not need time", {
  result = compute_cadence_bands(
    dplyr::group_by(simulated_nhanes_steps, PAXDAYM)
  )
  expect_true(dplyr::is_grouped_df(result))
  expect_equal(dplyr::group_vars(result), "PAXDAYM")
  expect_no_error(compute_cadence_bands(data.frame(steps = 1:3)))
})

test_that("compute_cadence_bands masks non-wear", {
  df = data.frame(steps = c(0, 50, 150), wear = c(TRUE, FALSE, TRUE))
  result = compute_cadence_bands(df, mask_wear = TRUE)
  expect_equal(as.character(result$cadence_band),
               c("zero", NA, "faster_ambulation"))
  expect_error(compute_cadence_bands(data.frame(steps = 1), mask_wear = TRUE),
               "wear")
})

test_that("summarize_cadence_bands counts every band, including empty ones", {
  df = data.frame(id = c(1, 1, 1, 2, 2),
                  steps = c(0, 0, 130, NA, 25))
  result = summarize_cadence_bands(compute_cadence_bands(df),
                                   group_cols = "id")
  expect_named(result, c("id", all_bands))
  expect_equal(result$zero, c(2L, 0L))
  expect_equal(result$sporadic, c(0L, 1L))
  expect_equal(result$faster_ambulation, c(1L, 0L))
  expect_equal(unname(rowSums(result[, all_bands])), c(3, 1))

  # all-NA groups are kept, with 0 minutes in every band
  result = summarize_cadence_bands(
    compute_cadence_bands(data.frame(id = 1:2, steps = c(NA, 10))),
    group_cols = "id"
  )
  expect_equal(nrow(result), 2L)
  expect_equal(unname(unlist(result[1, all_bands])), rep(0L, 8))
})

test_that("summarize_cadence_bands uses dplyr groups and no grouping", {
  bands = compute_cadence_bands(
    dplyr::group_by(simulated_nhanes_steps, SEQN, PAXDAYM)
  )
  expect_equal(
    summarize_cadence_bands(bands),
    summarize_cadence_bands(dplyr::ungroup(bands),
                            group_cols = nhanes_group_cols())
  )
  overall = summarize_cadence_bands(dplyr::ungroup(bands))
  expect_equal(nrow(overall), 1L)
  expect_equal(overall$zero, 6L * 1310L)
})

test_that("summarize_cadence_bands warns on non-factor bands", {
  bands = compute_cadence_bands(one_day())
  bands$cadence_band = as.character(bands$cadence_band)
  expect_warning(result <- summarize_cadence_bands(bands), "not a factor")
  expect_named(result, all_bands)
  expect_error(summarize_cadence_bands(data.frame(steps = 1)), "cadence_band")
  expect_error(summarize_cadence_bands(list(cadence_band = "zero")),
               "data frame")
  expect_error(summarize_cadence_bands(bands, group_cols = "nope"),
               "Missing required columns: nope")
})

test_that("ensure_all_cadence_bands adds missing bands as 0", {
  result = ensure_all_cadence_bands(data.frame(id = 1, zero = 5))
  expect_named(result, c("id", all_bands))
  expect_equal(result$zero, 5)
  expect_equal(result$brisk_walk, 0)

  result = ensure_all_cadence_bands(data.frame(id = 1), "running")
  expect_named(result, c("id", "running"))
})
