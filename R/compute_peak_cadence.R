
#' @title Compute Peak Cadence Metrics
#' @description Computes 1-minute, 30-minute, and 60-minute peak cadence metrics.
#' @param df A `data.frame` of data, which must have the columns of
#' `time` and `steps`.
#' @param group_cols columns to group the data by before computing cadence metrics
#' @return A data.table with peak cadence metrics added.
#' @export
compute_cadence_peak <- function(df,
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


#' @export
#' @rdname compute_cadence_peak
compute_cadence_level <- function(df,
                                 group_cols = c("SEQN", "PAXDAYM")
) {
  # issues with no symbol in the environment
  n_in_cadence_band = cadence = n = steps = NULL
  rm(list = c("n_in_cadence_band", "cadence", "n", "steps"))
  assertthat::assert_that(
    assertthat::has_name(df, "steps")
  )
  validate_steps_data(df = df,
                      columns = c("time", "steps", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)

  df = df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cadence = cut(steps, breaks = c(0, 60, 120, Inf),
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





#' @export
#' @rdname compute_cadence_peak
compute_cadence_max <- function(df,
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





#' @note
#' For cadence means using `compute_cadence_mean`, the columns
#' `wear` are necessary and `counts` are optional.  If `counts`
#' not available, then censored means are not reported.
#' @param counts_threshold Threshold for censoring steps, such that
#' steps where counts less than this threshold is not counted.
#'
#' @export
#' @rdname compute_cadence_peak
compute_cadence_mean <- function(df,
                                 group_cols = c("SEQN", "PAXDAYM"),
                                 counts_threshold = 500
) {
  # issues with no symbol in the environment
  counts = n_minutes = n_steps_censored = n_steps = steps = NULL
  mean_uncensored_cadence = mean_censored_cadence = NULL
  steps_censored = wear = NULL
  rm(list = c("steps", "n_steps",
              "n_steps_censored", "n_minutes", "counts",
              "mean_uncensored_cadence", "mean_censored_cadence",
              "steps_censored", "wear"))
  validate_steps_data(df = df,
                      columns = c("time", "steps", "wear", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)
  assertthat::assert_that(
    is.logical(df$wear) |
      all(stats::na.omit(unique(df$wear)) %in% c(0, 1))
  )
  if (assertthat::has_name(df, "counts")) {
    assertthat::assert_that(
      all(df$counts > 0, na.rm = TRUE),
    )
    df = df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(steps_censored = dplyr::if_else(counts < counts_threshold,
                                                    NA, steps)
      )
  } else {
    df = df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(steps_censored = NA)
  }

  df = df %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      n_minutes = sum(wear > 0, na.rm = TRUE),
      n_steps = sum(steps, na.rm = TRUE),
      n_steps_censored = sum(steps_censored, na.rm = TRUE),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      mean_uncensored_cadence = n_steps / n_minutes,
      mean_uncensored_cadence = dplyr::if_else(
        is.infinite(mean_uncensored_cadence),
        NA, mean_uncensored_cadence),

      mean_censored_cadence = n_steps_censored / n_minutes,
      mean_censored_cadence = dplyr::if_else(
        is.infinite(mean_censored_cadence),
        NA, mean_censored_cadence)
    ) %>%
    dplyr::select(-n_steps, -n_minutes, -n_steps_censored)

  if (assertthat::has_name(df, "counts")) {

  }
  return(df)
}
