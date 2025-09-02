
#' @title Compute Peak Cadence Metrics
#' @description Computes 1-minute, 30-minute, and 60-minute peak cadence metrics.
#' @param df A `data.frame` of data, which must have the columns of
#' `time` and `cadence`.
#' @param group_cols columns to group the data by before computing cadence metrics
#' @return A data.table with peak cadence metrics added.
#' @export
compute_cadence_peak <- function(df,
                                 group_cols = c("SEQN", "PAXDAYM")
) {
  # issues with no symbol in the environment
  cadence = NULL
  rm(list = c("cadence"))
  assertthat::assert_that(
    assertthat::has_name(df, "cadence")
  )
  validate_steps_data(df = df,
                      columns = c("time", "cadence", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)

  get_top = function(x, n = 30) {
    utils::head(sort(x, decreasing = TRUE), n = n)
  }

  df = df %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      # NEED CHECK FOR 1 minute times
      peak_1min = max(cadence, na.rm = TRUE),
      peak_30min = mean(get_top(cadence, 30L), na.rm = TRUE),
      peak_60min = mean(get_top(cadence, 60L), na.rm = TRUE),
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
    assertthat::has_name(df, "cadence")
  )
  validate_steps_data(df = df,
                      columns = c("time", "cadence", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)

  df = df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cadence = cut(cadence, breaks = c(0, 60, 120, Inf),
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
  cadence = NULL
  rm(list = c("cadence"))
  assertthat::assert_that(
    assertthat::has_name(df, "cadence")
  )
  validate_steps_data(df = df,
                      columns = c("time", "cadence", group_cols),
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
      max_5min = roll_data(cadence, 5L),
      max_20min = roll_data(cadence, 20L),
      max_30min = roll_data(cadence, 30L),
      max_60min = roll_data(cadence, 60L)
    ) %>%
    dplyr::ungroup()

  return(df)
}
