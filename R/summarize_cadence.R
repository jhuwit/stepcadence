#' @title Summarize Cadence Metrics
#' @description Computes all cadence metrics, from [compute_cadence_peak()],
#' [compute_cadence_mean()], [compute_cadence_max()],
#' [compute_cadence_level()], and [summarize_cadence_bands()], and joins them
#' into one row per group.
#' @inheritParams compute_cadence_level
#' @inheritParams compute_cadence_mean
#' @return A tibble with one row per group: the `group_cols`, followed by
#' the columns described in the functions above.
#' @export
#' @examples
#' summarize_cadence(simulated_nhanes_steps, group_cols = nhanes_group_cols())
#'
#' # a single day needs no grouping
#' one_day = subset(simulated_nhanes_steps, SEQN == "ID_1" & PAXDAYM == 1)
#' summarize_cadence(one_day)
#' @rdname summarize_cadence
summarize_cadence <- function(df,
                              mask_wear = FALSE,
                              step_thresholds = c(60, 120),
                              counts_threshold = 500,
                              group_cols = NULL
) {
  steps = NULL
  rm(list = c("steps"))
  step_thresholds = check_step_thresholds(step_thresholds)
  check_counts_threshold(counts_threshold)
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  df = x$df
  group_cols = x$group_cols

  bands = df |>
    dplyr::mutate(cadence_band = cut_cadence_bands(steps)) |>
    count_by_level(var = "cadence_band",
                   levels = cadence_band_labels(),
                   group_cols = group_cols)

  summaries = list(
    cadence_peak_worker(df, group_cols),
    cadence_mean_worker(df, group_cols, counts_threshold),
    cadence_max_worker(df, group_cols),
    cadence_level_worker(df, group_cols, step_thresholds),
    bands
  )
  join_summaries(summaries, group_cols)
}

#' @export
#' @rdname summarize_cadence
summarise_cadence <- summarize_cadence
