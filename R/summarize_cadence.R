#' @title Summarize Step Data
#' @description Calls all helper functions to compute cadence metrics and summarize results.
#' @inheritParams compute_cadence_level
#' @inheritParams compute_cadence_mean
#' @export
#' @examples
#' \dontrun{
#' summarize_cadence("path/to/file.csv.xz")
#' }
#' @export
#' @rdname summarize_cadence
summarize_cadence <- function(df,
                              mask_wear = FALSE,
                              step_thresholds = c(60, 120),
                              counts_threshold = 500,
                              group_cols = c("SEQN", "PAXDAYM")
) {
  peak <- compute_cadence_peak(df,
                               mask_wear = mask_wear,
                               group_cols = group_cols)
  max <- compute_cadence_max(df,
                             mask_wear = mask_wear,
                             group_cols = group_cols)
  mean <- compute_cadence_mean(df,
                             mask_wear = mask_wear,
                             group_cols = group_cols,
                             counts_threshold = counts_threshold
                             )

  levels = compute_cadence_level(df,
                                 mask_wear = mask_wear,
                                 group_cols = group_cols,
                                 step_thresholds = step_thresholds)

  bands = compute_cadence_bands(df,
                                mask_wear = mask_wear
  )
  bands = summarize_cadence_bands(bands, group_cols = group_cols)
  result <- peak %>%
    dplyr::full_join(mean) %>%
    dplyr::full_join(max) %>%
    dplyr::full_join(levels) %>%
    dplyr::full_join(bands)

  return(result)
}

#' @export
#' @rdname summarize_cadence
summarise_cadence <- summarize_cadence
