# Shared set-up for the summary functions: resolve groups, validate,
# mask non-wear minutes, and return an ungrouped tibble
prepare_steps_data = function(df, mask_wear, group_cols) {
  check_flag(mask_wear, "mask_wear")
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  group_cols = resolve_group_cols(df, group_cols)
  validate_steps_data(df = df,
                      columns = c("time", "steps", group_cols),
                      check_minute = TRUE,
                      group_cols = group_cols)
  df = dplyr::as_tibble(df)
  if (mask_wear) {
    df = df %>%
      mask_nonwear_steps()
  }
  list(df = df, group_cols = group_cols)
}

# mean of the n highest (not necessarily consecutive) minutes;
# NA if fewer than n minutes have steps
peak_minutes = function(x, n) {
  x = x[!is.na(x)]
  if (length(x) < n) {
    return(NA_real_)
  }
  mean(sort(x, decreasing = TRUE)[seq_len(n)])
}

# highest mean over n consecutive minutes; windows that contain a
# missing minute are skipped; NA if no complete window exists
max_rolling = function(x, n) {
  if (length(x) < n) {
    return(NA_real_)
  }
  out = data.table::frollmean(as.numeric(x), n, fill = NA)
  if (all(is.na(out))) {
    return(NA_real_)
  }
  max(out, na.rm = TRUE)
}

cadence_peak_worker = function(df, group_cols) {
  steps = NULL
  rm(list = c("steps"))
  df %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      peak_1min = peak_minutes(steps, 1L),
      peak_30min = peak_minutes(steps, 30L),
      peak_60min = peak_minutes(steps, 60L),
      .groups = "drop"
    )
}

cadence_level_worker = function(df, group_cols, step_thresholds) {
  steps = NULL
  rm(list = c("steps"))
  levels = paste0(c("low", "moderate", "high"), "_cadence")
  df %>%
    dplyr::mutate(
      .cadence_level = cut(steps, breaks = c(0, step_thresholds, Inf),
                           include.lowest = TRUE,
                           right = FALSE,
                           labels = levels)
    ) %>%
    count_by_level(var = ".cadence_level",
                   levels = levels,
                   group_cols = group_cols)
}

cadence_max_worker = function(df, group_cols) {
  steps = NULL
  rm(list = c("steps"))
  df %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      max_5min = max_rolling(steps, 5L),
      max_20min = max_rolling(steps, 20L),
      max_30min = max_rolling(steps, 30L),
      max_60min = max_rolling(steps, 60L),
      .groups = "drop"
    )
}

cadence_mean_worker = function(df, group_cols, counts_threshold) {
  counts = steps = steps_censored = wear = NULL
  rm(list = c("counts", "steps", "steps_censored", "wear"))
  has_counts = assertthat::has_name(df, "counts")
  has_wear = assertthat::has_name(df, "wear")
  if (has_counts) {
    assertthat::assert_that(
      is.numeric(df$counts),
      all(df$counts >= 0, na.rm = TRUE),
      msg = "`counts` must be numeric and non-negative."
    )
    # censored steps count as 0 steps but the minute stays in the denominator;
    # missing (or masked) steps stay missing
    df = df %>%
      dplyr::mutate(
        steps_censored = dplyr::if_else(
          counts < counts_threshold & !is.na(steps),
          0, as.numeric(steps))
      )
  } else {
    df = df %>%
      dplyr::mutate(steps_censored = NA_real_)
  }
  if (has_wear) {
    check_wear(df$wear)
  } else {
    df = df %>%
      dplyr::mutate(wear = NA)
  }

  df = df %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      n_minutes = dplyr::n(),
      n_minutes_wear = sum(wear > 0, na.rm = TRUE),
      n_minutes_valid = sum(!is.na(steps)),
      mean_uncensored_cadence = safe_mean(steps),
      mean_censored_cadence = safe_mean(steps_censored),
      .groups = "drop"
    )
  if (!has_wear) {
    df = df %>%
      dplyr::mutate(n_minutes_wear = NA_integer_)
  }
  df
}

#' @title Compute Peak Cadence Metrics
#' @description Computes peak 1-minute, 30-minute, and 60-minute cadence:
#' the mean steps/min of the 1, 30, or 60 highest, but not necessarily
#' consecutive, minutes (Tudor-Locke et al., 2018).
#' For the highest *consecutive* minutes, see [compute_cadence_max()].
#' @param df A data frame of minute-level data with columns:
#' * `time`: numeric minutes or a date-time (`POSIXct`), increasing by
#'   exactly 1 minute within each group, with no gaps.
#' * `steps`: numeric, non-negative steps in that minute.  `NA` is allowed
#'   and treated as a missing minute.
#' * `wear` (optional; required if `mask_wear = TRUE`): logical (`TRUE` =
#'   worn) or 0/1 (1 = worn).
#' * any columns named in `group_cols`.
#' @param mask_wear If `TRUE`, steps in non-wear minutes (`wear` is
#' `FALSE`/0) are set to `NA` and excluded from all metrics.  Requires a
#' `wear` column.
#' @param group_cols Character vector of columns that identify one unit of
#' analysis, such as a participant-day; metrics are computed separately for
#' each combination.  The default, `NULL`, uses the groups of `df` if it was
#' grouped with [dplyr::group_by()], and otherwise treats all of `df` as a
#' single unit.  See [nhanes_group_cols()] for NHANES data.
#' @return A tibble with one row per group, the `group_cols`, and:
#' * `peak_1min`: highest steps in a single minute.
#' * `peak_30min`, `peak_60min`: mean of the 30 or 60 highest minutes.
#'
#' A metric is `NA` if the group has fewer non-missing minutes than its
#' window (e.g. fewer than 30 for `peak_30min`).
#' @references
#' Tudor-Locke C, Han H, Aguiar EJ, et al. How fast is fast enough? Walking
#' cadence (steps/min) as a practical estimate of intensity in adults: a
#' narrative review. *British Journal of Sports Medicine*. 2018;52:776-788.
#' \doi{10.1136/bjsports-2017-097628}
#' @export
#' @rdname compute_cadence_peak
#' @examples
#' compute_cadence_peak(simulated_nhanes_steps,
#'                      group_cols = nhanes_group_cols())
#'
#' # a single day needs no grouping
#' one_day = subset(simulated_nhanes_steps, SEQN == "ID_1" & PAXDAYM == 1)
#' compute_cadence_peak(one_day)
compute_cadence_peak <- function(df,
                                 mask_wear = FALSE,
                                 group_cols = NULL
) {
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  cadence_peak_worker(x$df, x$group_cols)
}


#' @title Compute Time in Cadence Levels
#' @description Counts the minutes at low, moderate, and high cadence.
#' @inheritParams compute_cadence_peak
#' @param step_thresholds Two different positive numbers (steps/min) that
#' split cadence into levels: low is from 0 up to, but not including, the
#' lower threshold; moderate is from the lower up to, but not including, the
#' upper threshold; and high is the upper threshold or more.  The default,
#' `c(60, 120)`, gives low < 60, moderate 60-119, and high >= 120.
#' @return A tibble with one row per group, the `group_cols`, and integer
#' columns `low_cadence`, `moderate_cadence`, and `high_cadence` giving the
#' number of minutes at each level.  Minutes with missing steps are not
#' counted.
#' @export
#' @examples
#' compute_cadence_level(simulated_nhanes_steps,
#'                       group_cols = nhanes_group_cols())
#' compute_cadence_level(simulated_nhanes_steps,
#'                       step_thresholds = c(30, 100),
#'                       group_cols = nhanes_group_cols())
compute_cadence_level <- function(df,
                                  mask_wear = FALSE,
                                  step_thresholds = c(60, 120),
                                  group_cols = NULL
) {
  step_thresholds = check_step_thresholds(step_thresholds)
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  cadence_level_worker(x$df, x$group_cols, step_thresholds)
}


#' @title Compute Maximum Consecutive-Minute Cadence
#' @description Computes the highest mean steps/min over 5, 20, 30, and 60
#' *consecutive* minutes (Tudor-Locke et al., 2018).  For the highest minutes
#' regardless of order, see [compute_cadence_peak()].
#' @inheritParams compute_cadence_peak
#' @return A tibble with one row per group, the `group_cols`, and
#' `max_5min`, `max_20min`, `max_30min`, and `max_60min`.  A window that
#' contains a missing minute (e.g. masked non-wear) is skipped.  A metric is
#' `NA` if the group has no complete window of that length.
#' @export
#' @examples
#' compute_cadence_max(simulated_nhanes_steps,
#'                     group_cols = nhanes_group_cols())
compute_cadence_max <- function(df,
                                mask_wear = FALSE,
                                group_cols = NULL
) {
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  cadence_max_worker(x$df, x$group_cols)
}


#' @title Compute Mean Cadence
#' @description Computes uncensored and censored mean cadence (steps/min)
#' following Tudor-Locke et al. (2018):
#' * uncensored: total steps divided by the number of minutes with steps;
#' * censored: the same, after setting steps to 0 in minutes where
#'   activity `counts` are below `counts_threshold`.
#'
#' The paper divides by device wear time.  Use `mask_wear = TRUE` so that
#' non-wear minutes are excluded from both totals and the denominator.
#' @inheritParams compute_cadence_peak
#' @param df A data frame as described in [compute_cadence_peak()].  It may
#' also have a `counts` column of non-negative activity counts per minute,
#' which is needed for the censored mean.
#' @param counts_threshold Activity counts/min below which a minute's steps
#' are censored (set to 0) for `mean_censored_cadence`.
#' @return A tibble with one row per group, the `group_cols`, and:
#' * `n_minutes`: number of rows (minutes).
#' * `n_minutes_wear`: number of wear minutes; `NA` if `df` has no `wear`
#'   column.
#' * `n_minutes_valid`: number of minutes with non-missing steps (after
#'   masking); the denominator of both means.
#' * `mean_uncensored_cadence`: mean steps/min.
#' * `mean_censored_cadence`: mean steps/min after censoring; `NA` if `df`
#'   has no `counts` column.  Minutes with missing `counts` are excluded.
#'
#' Means are `NA` if there are no minutes with steps.
#' @export
#' @examples
#' df = simulated_nhanes_steps
#' set.seed(1)
#' df$counts = rpois(nrow(df), lambda = 500)
#' df$wear = TRUE
#' compute_cadence_mean(df, mask_wear = TRUE,
#'                      group_cols = nhanes_group_cols())
compute_cadence_mean <- function(df,
                                 mask_wear = FALSE,
                                 counts_threshold = 500,
                                 group_cols = NULL
                                 ) {
  check_counts_threshold(counts_threshold)
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  cadence_mean_worker(x$df, x$group_cols, counts_threshold)
}
