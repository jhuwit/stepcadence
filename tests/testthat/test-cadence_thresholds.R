test_that("cadence_thresholds returns the CADENCE table", {
  tab = cadence_thresholds()
  expect_s3_class(tab, "data.frame")
  expect_equal(tab$age_min, c(6, 9, 12, 15, 18, 21, 41, 61))
  expect_equal(tab$age_max, c(8, 11, 14, 17, 20, 40, 60, 85))
})

test_that("cadence_thresholds looks up by age", {
  expect_equal(cadence_thresholds(6), c(moderate = 125, vigorous = 155))
  expect_equal(cadence_thresholds(8.9), c(moderate = 125, vigorous = 155))
  expect_equal(cadence_thresholds(9), c(moderate = 115, vigorous = 130))
  expect_equal(cadence_thresholds(19), c(moderate = 90, vigorous = 125))
  expect_equal(cadence_thresholds(35), c(moderate = 100, vigorous = 130))
  expect_equal(cadence_thresholds(60), c(moderate = 100, vigorous = 130))
  expect_equal(cadence_thresholds(85.5), c(moderate = 100, vigorous = NA))
})

test_that("cadence_thresholds rejects bad ages", {
  for (bad in list(5, 86, NA_real_, c(30, 40), "30", Inf)) {
    expect_error(cadence_thresholds(bad), "age")
  }
})
