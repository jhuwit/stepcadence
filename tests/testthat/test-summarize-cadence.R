test_that("simulated_nhanes_steps check", {

  test_result <- summarize_cadence(simulated_nhanes_steps)

  print("Columns in test_result after function call:")
  print(names(test_result))  # Debugging step

  expect_true(all(c("SEQN", "PAXDAYM", "min_num", "steps") %in% colnames(simulated_nhanes_steps)),
              info = "Some expected columns are missing in test_result.")
})

test_that("summarize_cadence check", {

  test_result <- summarize_cadence(simulated_nhanes_steps)

  expect_named(
    test_result,
    c("SEQN", "PAXDAYM", "peak_1min", "peak_30min", "peak_60min",
      "moderate_cadence", "high_cadence", "max_5min", "max_20min",
      "max_30min", "max_60min", "Zero", "Incidental", "Sporadic", "Purposeful",
      "Slow Walk", "Very Fast Walk", "All-out", "Medium Walk", "Brisk Walk",
      "Fast Walk"),
    ignore.order = TRUE,
    info = "Some expected columns are missing in test_result.")
})

test_that("summarize_accelerometry produces expected results", {
  # File paths (update if running in a different environment)
  expected_data_path <- testthat::test_path("testdata", "expected_cadence_metrics.csv")

  # Load and run function
  test_result <- summarize_cadence(simulated_nhanes_steps)

  # Read expected results
  expected_result <- data.table::fread(expected_data_path)

  # Ensure column names match before comparing values
  expect_equal(sort(names(test_result)), sort(names(expected_result)),
               info = "Column names do not match.")

  # Sort both datasets by ID and day to ensure correct row comparison
  setorder(test_result, SEQN, PAXDAYM)
  setorder(expected_result, SEQN, PAXDAYM)

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
