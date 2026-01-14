
#' @title Compute Cadence Bands
#' @description Categorizes steps per minute into cadence bands.
#' @inheritParams compute_cadence_peak
#' @export
#' @return A list containing the updated data.table and a summary of cadence bands.
compute_cadence_bands <- function(df,
                                  mask_wear = FALSE
                                  ) {
  steps = NULL
  rm(list = c("steps"))
  # all_bands <- c(
  #   "Zero",
  #   "Incidental",
  #   "Sporadic",
  #   "Purposeful",
  #   "Slow Walk",
  #   "Medium Walk",
  #   "Brisk Walk",
  #   "Fast Walk",
  #   "Very Fast Walk",
  #   "All-out"
  # )
  all_bands = c("zero", "incidental", "sporadic", "purposeful", "slow_walk",
                "medium_walk", "brisk_walk", "fast_walk", "very_fast_walk", "all_out"
  )

  assertthat::assert_that(
    assertthat::has_name(df, "steps")
  )
  validate_steps_data(df = df,
                      columns = c("steps"),
                      check_minute = FALSE)
  if (mask_wear) {
    df = df %>%
      mask_nonwear_steps()
  }
  gv = dplyr::group_vars(df)
  df = df %>%
    # ungroup for speed
    dplyr::ungroup() %>%
    dplyr::mutate(
      cadence_band = cut(
        steps,
        breaks = c(0, 1, 19, 29, 49, 59, 79, 89, 99, 119, Inf),
        labels = all_bands,
        include.lowest = TRUE
      )
    )

  if (length(gv) > 0) {
    # regroup
    df = df %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(gv))
      )
  }

  df
}

#' @rdname compute_cadence_bands
#' @export
summarize_cadence_bands = function(df,
                                   group_cols = c("SEQN", "PAXDAYM")
                                   ) {
  n_in_cadence_band = n = cadence_band = NULL
  rm(list = c("cadence_band", "n", "n_in_cadence_band"))
  assertthat::assert_that(
    assertthat::has_name(df, "cadence_band")
  )
  if (!is.factor(df$cadence_band)) {
    warning("Cadence bands is not a factor (should be), coercing")
    df = df %>%
      dplyr::mutate(cadence_band = factor(cadence_band))
  }

  band_summary = df %>%
    group_string_cols(group_cols) %>%
    # will keep all bands since .drop = FALSE
    dplyr::count(cadence_band, name = "n_in_cadence_band", .drop = FALSE) %>%
    tidyr::pivot_wider(
      names_from = cadence_band,
      values_from = n_in_cadence_band,
      values_fill = 0)
  return(band_summary)
}

#' @rdname compute_cadence_bands
#' @export
summarise_cadence_bands = summarize_cadence_bands

#' @title Ensure All Cadence Bands Exist
#' @description Ensures that all cadence bands appear in the output summary.
#' @param band_summary Data.table with cadence band counts.
#' @param all_bands Vector of all expected cadence bands.
#' @return A data.table with all cadence bands present.
#' @export
ensure_all_cadence_bands <- function(band_summary, all_bands) {
  for (band in all_bands) {
    if (!(band %in% names(band_summary))) {
      band_summary[[band]] <- 0
    }
  }
  return(band_summary)
}
