set.seed(20260114)
df = simulated_nhanes_steps %>%
  dplyr::mutate(wear = rbinom(n = nrow(simulated_nhanes_steps), size = 1,
                              prob = 0.1))

test_that("simulated_nhanes_steps with apply_mask check", {

  test_result <- summarize_cadence(df, mask_wear = TRUE)

  print("Columns in test_result after function call:")
  print(names(test_result))  # Debugging step

  expect_true(all(c("SEQN", "PAXDAYM", "time", "steps") %in% colnames(simulated_nhanes_steps)),
              info = "Some expected columns are missing in test_result.")

  testthat::expect_equal(
    test_result$peak_30min,
    c(73.6666666666667, 75, 43.3333333333333, 46, 53.6666666666667,
      54.6666666666667)
  )

  test_result_nomask <- summarize_cadence(df)
  testthat::expect_equal(
    test_result_nomask$peak_30min,
    c(200, 200, 200, 200, 200, 200)
  )


})
