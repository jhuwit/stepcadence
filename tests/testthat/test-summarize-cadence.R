test_that("simulated_nhanes_steps check", {

  test_result <- summarize_cadence(simulated_nhanes_steps, group_cols = nhanes_group_cols())

  print("Columns in test_result after function call:")
  print(names(test_result))  # Debugging step

  expect_true(all(c("SEQN", "PAXDAYM", "time", "steps") %in% colnames(simulated_nhanes_steps)),
              info = "Some expected columns are missing in test_result.")
})

test_that("summarize_cadence check", {

  test_result <- summarize_cadence(simulated_nhanes_steps, group_cols = nhanes_group_cols())

  expected_cols =
    c("SEQN", "PAXDAYM", "peak_1min", "peak_30min", "peak_60min",
      "n_minutes", "n_minutes_wear", "mean_uncensored_cadence", "mean_censored_cadence",
      "max_5min", "max_20min", "max_30min", "max_60min", "low_cadence",
      "moderate_cadence", "high_cadence", "zero", "incidental", "sporadic",
      "purposeful", "slow_walk", "medium_walk", "brisk_walk",
      # "fast_walk",
      # "very_fast_walk",
      "all_out")
  expect_named(
    test_result,
    expected = expected_cols,
    ignore.order = TRUE)
})

test_that("ensure_all_cadence_bands adding in fake band", {

  test_result <- summarize_cadence(simulated_nhanes_steps, group_cols = nhanes_group_cols())

  test_result = ensure_all_cadence_bands(test_result, "running_for_your_life")
  expected_cols =
    c("SEQN", "PAXDAYM", "peak_1min", "peak_30min", "peak_60min",
      "n_minutes", "n_minutes_wear", "mean_uncensored_cadence", "mean_censored_cadence",
      "max_5min", "max_20min", "max_30min", "max_60min", "low_cadence",
      "moderate_cadence", "high_cadence", "zero", "incidental", "sporadic",
      "purposeful", "slow_walk", "medium_walk", "brisk_walk",
      # "fast_walk",
      # "very_fast_walk",
      "all_out",
      "running_for_your_life")
  expect_named(
    test_result,
    expected = expected_cols,
    ignore.order = TRUE)
})

test_that("summarize_accelerometry produces expected results", {
  testthat::skip_if_not_installed("janitor")
  # File paths (update if running in a different environment)
  expected_data_path <- testthat::test_path("testdata", "expected_cadence_metrics.csv")

  # Load and run function
  test_result <- summarize_cadence(simulated_nhanes_steps, group_cols = nhanes_group_cols())

  # Read expected results
  expected_result <- data.table::fread(expected_data_path)

  expected_result$V1 = NULL
  expected_result = expected_result %>%
    janitor::clean_names() %>%
    dplyr::rename(SEQN=seqn, PAXDAYM = paxdaym)
  test_result = test_result[, colnames(expected_result)]
  # Ensure column names match before comparing values
  expect_equal(sort(names(test_result)), sort(names(expected_result)),
               info = "Column names do not match.")

  # Sort both datasets by ID and day to ensure correct row comparison
  data.table::setorder(test_result, SEQN, PAXDAYM)
  data.table::setorder(expected_result, SEQN, PAXDAYM)

  # Check if number of rows match
  expect_equal(nrow(test_result), nrow(expected_result),
               info = "Number of rows in the output and expected results differ.")

  # Check if values match (allowing some tolerance for floating-point errors)
  common_cols <- intersect(names(test_result), names(expected_result))
  for (col in common_cols) {
    expect_equal(test_result[[col]], expected_result[[col]], tolerance = 1e-5,
                 info = paste("Mismatch in column:", col))
  }
})
