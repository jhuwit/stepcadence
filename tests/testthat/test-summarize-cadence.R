expected_cols = c(
  "peak_1min", "peak_30min", "peak_60min",
  "n_minutes", "n_minutes_wear", "n_minutes_valid",
  "mean_uncensored_cadence", "mean_censored_cadence",
  "max_5min", "max_20min", "max_30min", "max_60min",
  "low_cadence", "moderate_cadence", "high_cadence",
  "zero", "incidental", "sporadic", "purposeful", "slow_walk",
  "medium_walk", "brisk_walk", "faster_ambulation"
)

test_that("summarize_cadence matches hand-computed values", {
  # Each simulated day is 0 steps except: 30 min at 50, 100, 120, and 200;
  # 5 min at 10, 20, 30, 40, 50; and 5 min at 200, 190, 180, 170, 160.
  # e.g. peak_60min = (31 * 200 + 190 + 180 + 170 + 160 + 25 * 120) / 60 = 165
  # and mean = 15150 / 1440.  See testdata/expected_cadence_metrics.csv.
  expected = utils::read.csv(
    testthat::test_path("testdata", "expected_cadence_metrics.csv"),
    colClasses = c(n_minutes_wear = "integer",
                   mean_censored_cadence = "numeric")
  )
  result = summarize_cadence(simulated_nhanes_steps,
                             group_cols = nhanes_group_cols())

  expect_s3_class(result, "tbl_df")
  expect_named(result, c(nhanes_group_cols(), expected_cols))
  result = as.data.frame(result[order(result$SEQN, result$PAXDAYM), ])
  rownames(result) = NULL
  expect_equal(result, expected, ignore_attr = TRUE, tolerance = 1e-8)
})

test_that("summarize_cadence works with the default (no) grouping", {
  expect_no_message(result <- summarize_cadence(one_day()))
  expect_equal(nrow(result), 1L)
  expect_named(result, expected_cols)

  grouped = summarize_cadence(simulated_nhanes_steps,
                              group_cols = nhanes_group_cols())
  expect_equal(
    unlist(result),
    unlist(grouped[1, expected_cols])
  )
})

test_that("summarize_cadence uses dplyr groups when group_cols is NULL", {
  grouped_df = dplyr::group_by(simulated_nhanes_steps, SEQN, PAXDAYM)
  expect_equal(
    summarize_cadence(grouped_df),
    summarize_cadence(simulated_nhanes_steps, group_cols = nhanes_group_cols())
  )
  # group_cols takes precedence over existing groups
  by_id = summarize_cadence(
    dplyr::group_by(simulated_nhanes_steps, SEQN),
    group_cols = nhanes_group_cols()
  )
  expect_equal(nrow(by_id), 6L)
})

test_that("summarize_cadence agrees with the individual functions", {
  df = simulated_nhanes_steps
  set.seed(20260114)
  df$wear = stats::rbinom(nrow(df), size = 1, prob = 0.9)
  df$counts = stats::rpois(nrow(df), lambda = 500)
  gc = nhanes_group_cols()
  result = summarize_cadence(df, mask_wear = TRUE, group_cols = gc,
                             step_thresholds = c(40, 100),
                             counts_threshold = 450)
  pieces = list(
    compute_cadence_peak(df, mask_wear = TRUE, group_cols = gc),
    compute_cadence_mean(df, mask_wear = TRUE, group_cols = gc,
                         counts_threshold = 450),
    compute_cadence_max(df, mask_wear = TRUE, group_cols = gc),
    compute_cadence_level(df, mask_wear = TRUE, group_cols = gc,
                          step_thresholds = c(40, 100)),
    summarize_cadence_bands(compute_cadence_bands(df, mask_wear = TRUE),
                            group_cols = gc)
  )
  expected = Reduce(function(x, y) dplyr::full_join(x, y, by = gc), pieces)
  expect_equal(result, expected)
})

test_that("masking non-wear does not create an NA column", {
  df = simulated_nhanes_steps
  set.seed(20260114)
  df$wear = stats::rbinom(nrow(df), size = 1, prob = 0.1)
  result = summarize_cadence(df, mask_wear = TRUE,
                             group_cols = nhanes_group_cols())
  expect_named(result, c(nhanes_group_cols(), expected_cols))

  # banded and levelled minutes both add up to the wear minutes
  band_total = rowSums(result[, c("zero", "incidental", "sporadic",
                                  "purposeful", "slow_walk", "medium_walk",
                                  "brisk_walk", "faster_ambulation")])
  level_total = rowSums(result[, c("low_cadence", "moderate_cadence",
                                   "high_cadence")])
  expect_equal(band_total, result$n_minutes_wear)
  expect_equal(level_total, result$n_minutes_wear)
  expect_equal(result$n_minutes_valid, result$n_minutes_wear)
  expect_true(all(result$peak_30min < 200))
})

test_that("summarise_cadence is an alias", {
  expect_identical(summarise_cadence, summarize_cadence)
  expect_identical(summarise_cadence_bands, summarize_cadence_bands)
})

test_that("summarize_cadence validates its threshold arguments", {
  expect_error(summarize_cadence(one_day(), step_thresholds = 60),
               "step_thresholds")
  expect_error(summarize_cadence(one_day(), counts_threshold = -1),
               "counts_threshold")
  expect_error(summarize_cadence(one_day(), mask_wear = NA), "mask_wear")
})
