
#' @title Compute Peak Cadence Metrics
#' @description Computes 1-minute, 30-minute, and 60-minute peak cadence metrics.
#' @param df A `data.frame` of data, which must have the columns of
#' `time` and `steps`.  If `group_cols` added, they must be included in the data
#' as well.  If `mask_wear = TRUE`, the `wear` column must also be included, which
#' should be a logical indicator of wear (`TRUE`) or non-wear (`FALSE`) or
#' a flag of 0/1
#' @param mask_wear if `TRUE`, then steps that are non-wear will be set to `NA`.
#' @param group_cols columns to group the data by before computing metrics
#' @return A data.table with peak cadence metrics added.
#' @export
#' @rdname compute_cadence_peak
compute_cadence_peak <- function(df,
                                 mask_wear = FALSE,
                                 group_cols = c("SEQN", "PAXDAYM")
) {
  # issues with no symbol in the environment
  steps = NULL
  rm(list = c("steps"))
  assertthat::assert_that(
    assertthat::has_name(df, "steps")
  )
  validate_steps_data(df = df,
                      columns = c("time", "steps", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)

  if (mask_wear) {
    df = df %>%
      mask_nonwear_steps()
  }
  get_top = function(x, n = 30) {
    utils::head(sort(x, decreasing = TRUE), n = n)
  }

  df = df %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      # NEED CHECK FOR 1 minute times
      peak_1min = max(steps, na.rm = TRUE),
      peak_30min = mean(get_top(steps, 30L), na.rm = TRUE),
      peak_60min = mean(get_top(steps, 60L), na.rm = TRUE),
    ) %>%
    dplyr::ungroup()
  return(df)
}


#' Compute Cadence Peak
#'
#' @inheritParams compute_cadence_peak
#' @export
#' @param step_thresholds a vector of two unique values to define the cadence
#' bands for moderate and high.  0 to first threshold is always
#' set for low and above the second threshold is always set for high.
compute_cadence_level <- function(df,
                                  mask_wear = FALSE,
                                  step_thresholds = c(60, 120),
                                  group_cols = c("SEQN", "PAXDAYM")
) {
  # issues with no symbol in the environment
  n_in_cadence_band = cadence = n = steps = NULL
  rm(list = c("n_in_cadence_band", "cadence", "n", "steps"))
  assertthat::assert_that(
    assertthat::has_name(df, "steps")
  )
  assertthat::assert_that(
    !anyNA(step_thresholds),
    length(step_thresholds) == 2L,
    length(unique(step_thresholds)) == 2L
  )
  validate_steps_data(df = df,
                      columns = c("time", "steps", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)
  if (mask_wear) {
    df = df %>%
      mask_nonwear_steps()
  }

  df = df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cadence = cut(steps, breaks = c(0, step_thresholds, Inf),
                    include.lowest = TRUE,
                    right = FALSE,
                    labels = paste0(c("low", "moderate", "high"), "_cadence")
      )
    ) %>%
    group_string_cols(c(group_cols, "cadence")) %>%
    dplyr::count(name = "n_in_cadence_band")
  df = df %>%
    tidyr::pivot_wider(names_from = cadence,
                       values_from = n_in_cadence_band,
                       values_fill = 0) %>%
    dplyr::ungroup()
  return(df)
}





#' Compute Cadence Max
#'
#' @inheritParams compute_cadence_peak
#' @export
compute_cadence_max <- function(df,
                                mask_wear = FALSE,
                                group_cols = c("SEQN", "PAXDAYM")
) {
  # issues with no symbol in the environment
  steps = NULL
  rm(list = c("steps"))
  assertthat::assert_that(
    assertthat::has_name(df, "steps")
  )
  validate_steps_data(df = df,
                      columns = c("time", "steps", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)
  if (mask_wear) {
    df = df %>%
      mask_nonwear_steps()
  }

  roll_data =  function(x, n = 30) {
    out = data.table::frollmean(x, n, align = "center", fill = 0)
    max(out, na.rm = TRUE)
  }

  df = df %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      # NEED CHECK FOR 1 minute times
      max_5min = roll_data(steps, 5L),
      max_20min = roll_data(steps, 20L),
      max_30min = roll_data(steps, 30L),
      max_60min = roll_data(steps, 60L)
    ) %>%
    dplyr::ungroup()

  return(df)
}




#' Compute Cadence Mean
#'
#' @inheritParams compute_cadence_peak
#' @param counts_threshold Threshold for censoring steps, such that
#' steps where counts less than this threshold is not counted.
#'
#' @export
#' @note
#' For cadence means using `compute_cadence_mean`, the columns
#' `wear` are necessary and `counts` are optional.  If `counts`
#' not available, then censored means are not reported.
compute_cadence_mean <- function(df,
                                 mask_wear = FALSE,
                                 counts_threshold = 500,
                                 group_cols = c("SEQN", "PAXDAYM")
                                 ) {
  # issues with no symbol in the environment
  counts = n_minutes = n_steps_censored = n_steps = steps = NULL
  n_minutes_wear = mean_uncensored_cadence = mean_censored_cadence = NULL
  steps_censored = wear = NULL
  rm(list = c("steps", "n_steps",
              "n_steps_censored", "n_minutes", "counts",
              "mean_uncensored_cadence", "mean_censored_cadence",
              "steps_censored", "wear", "n_minutes_wear"))
  validate_steps_data(df = df,
                      columns = c("time", "steps", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)

  if (mask_wear) {
    df = df %>%
      mask_nonwear_steps()
  }
  if (assertthat::has_name(df, "counts")) {
    assertthat::assert_that(
      all(df$counts >= 0, na.rm = TRUE)
    )
    df = df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(steps_censored = dplyr::if_else(counts < counts_threshold,
                                                    NA_real_, steps)
      )
  } else {
    df = df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(steps_censored = NA)
  }
  has_wear = assertthat::has_name(df, "wear")
  if (!has_wear) {
    df = df %>%
      dplyr::mutate(wear = NA)
  }


  df = df %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      n_minutes = dplyr::n(),
      n_minutes_wear = sum(wear, na.rm = TRUE),
      mean_uncensored_cadence = mean(steps, na.rm = TRUE),
      mean_censored_cadence = mean(steps_censored, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      mean_uncensored_cadence = dplyr::if_else(
        is.infinite(mean_uncensored_cadence),
        NA, mean_uncensored_cadence),

      n_minutes_wear = dplyr::if_else(
        is.infinite(n_minutes_wear),
        NA, n_minutes_wear),

      mean_censored_cadence = dplyr::if_else(
        is.infinite(mean_censored_cadence),
        NA, mean_censored_cadence)
    )

  return(df)
}
