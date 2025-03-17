#' @title Summarize Step Data
#' @description Calls all helper functions to compute cadence metrics and summarize results.
#' @param file File path to the accelerometry dataset.
#' @param group_cols columns to group the data by before computing cadence metrics
#' @param ... additional arguments to pass to [load_accelerometry_data()]
#' @return A summarized data.table with cadence metrics.
#' @examples
#' \dontrun{
#' summarize_cadence("path/to/file.csv.xz")
#' }
#' @export
summarize_cadence_file <- function(
    file,
    ...,
    group_cols = c("SEQN", "PAXDAYM")
) {
  dt_long <- load_accelerometry_data(file, ...)

  result = summarize_cadence(dt_long)
  return(result)
}

#' @export
#' @rdname summarize_cadence_file
#' @param df data frame of accelerometry data
summarize_cadence <- function(df,
                              group_cols = c("SEQN", "PAXDAYM")
) {
  peak <- compute_cadence_peak(df, group_cols = group_cols)
  max <- compute_cadence_max(df, group_cols = group_cols)

  levels = compute_cadence_level(df, group_cols = group_cols)

  bands = compute_cadence_bands(df)
  bands = summarize_cadence_bands(bands, group_cols = group_cols)
  result <- peak %>%
    dplyr::full_join(max) %>%
    dplyr::full_join(levels) %>%
    dplyr::full_join(bands)

  return(result)
}

#' @export
#' @rdname summarize_cadence_file
summarise_cadence <- summarize_cadence
