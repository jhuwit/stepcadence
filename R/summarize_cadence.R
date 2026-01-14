#' @title Summarize Step Data
#' @description Calls all helper functions to compute cadence metrics and summarize results.
#' @param group_cols columns to group the data by before computing cadence metrics
#' @return A summarized data.table with cadence metrics.
#' @examples
#' \dontrun{
#' data = readr::read_csv("path/to/file.csv.xz")
#' summarize_cadence(data)
#' }
#' @export
#' @param df data frame of accelerometry data
#' @param step_thresholds a vector of two unique values to define the cadence
#' bands for moderate and high.  0 to first threshold is always
#' set for low and above the second threshold is always set for high.
#' @rdname summarize_cadence
summarize_cadence <- function(df,
                              group_cols = c("SEQN", "PAXDAYM"),
                              step_thresholds = c(60, 120)
) {
  peak <- compute_cadence_peak(df, group_cols = group_cols)
  max <- compute_cadence_max(df, group_cols = group_cols)

  levels = compute_cadence_level(df,
                                 group_cols = group_cols,
                                 step_thresholds = step_thresholds)

  bands = compute_cadence_bands(df)
  bands = summarize_cadence_bands(bands, group_cols = group_cols)
  result <- peak %>%
    dplyr::full_join(max) %>%
    dplyr::full_join(levels) %>%
    dplyr::full_join(bands)

  return(result)
}

#' @export
#' @rdname summarize_cadence
summarise_cadence <- summarize_cadence
