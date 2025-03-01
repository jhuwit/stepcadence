#' @title Summarize Step Data
#' @description Calls all helper functions to compute cadence metrics and summarize results.
#' @param filepath File path to the accelerometry dataset.
#' @return A summarized data.table with cadence metrics.
#' @examples summarize_cadence("path/to/file.csv.xz")
#' @export
summarize_cadence <- function(filepath) {
  dt_long <- load_accelerometry_data(filepath) %>% setDT()

  dt_long <- compute_peak_cadence(dt_long) %>% setDT()
  dt_long <- compute_max_cadence(dt_long) %>% setDT()

  band_results <- compute_cadence_bands(dt_long)
  band_summary <- ensure_all_cadence_bands(band_results$band_summary, band_results$all_bands)

  dt_long[, moderate_cadence := sum(steps >= 60 & steps < 120, na.rm = TRUE), by = .(SEQN, PAXDAYM)]
  dt_long[, high_cadence := sum(steps >= 120, na.rm = TRUE), by = .(SEQN, PAXDAYM)]

  result <- dt_long %>%
    select(SEQN, PAXDAYM, peak_1min, peak_30min, peak_60min, moderate_cadence, high_cadence,
           max_5min, max_20min, max_30min, max_60min) %>%
    distinct()

  result <- result %>% left_join(band_summary, by = c("SEQN", "PAXDAYM"))
  return(result)
}
