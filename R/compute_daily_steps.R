daily_steps_worker = function(df, group_cols) {
  steps = NULL
  rm(list = c("steps"))
  df |>
    group_string_cols(group_cols) |>
    dplyr::summarise(
      total_steps = safe_sum(steps),
      n_minutes_stepping = sum(steps > 0, na.rm = TRUE),
      mean_stepping_cadence = safe_mean(steps[!is.na(steps) & steps > 0]),
      .groups = "drop"
    )
}

#' @title Compute Total Steps and Stepping Time
#' @description Computes total steps, stepping time (minutes with any
#' steps), and mean cadence while stepping for each group.  With
#' participant-day groups, such as [nhanes_group_cols()], these are
#' steps/day and stepping minutes/day.  Use [step_volume_category()] to
#' classify mean steps/day.
#' @inheritParams compute_cadence_peak
#' @return A tibble with one row per group, the `group_cols`, and:
#' * `total_steps`: sum of steps; `NA` if no minute has steps.
#' * `n_minutes_stepping`: number of minutes with more than 0 steps.
#' * `mean_stepping_cadence`: mean steps/min over those minutes; `NA` if
#'   there are none.  Unlike `mean_uncensored_cadence` from
#'   [compute_cadence_mean()], minutes with 0 steps are excluded.
#'
#' Minutes with missing steps (including masked non-wear) are not counted,
#' so `total_steps` is not adjusted for missing data.
#' @references
#' Tudor-Locke C, Bassett DR Jr. How many steps/day are enough?
#' Preliminary pedometer indices for public health. *Sports Medicine*.
#' 2004;34(1):1-8. \doi{10.2165/00007256-200434010-00001}
#'
#' Saint-Maurice PF, Troiano RP, Bassett DR Jr, et al. Association of daily
#' step count and step intensity with mortality among US adults. *JAMA*.
#' 2020;323(12):1151-1160. \doi{10.1001/jama.2020.1382}
#'
#' Granat M, Clarke C, Holdsworth R, Stansfield B, Dall P. Quantifying the
#' cadence of free-living walking using event-based analysis. *Gait &
#' Posture*. 2015;42(1):85-90. \doi{10.1016/j.gaitpost.2015.04.012}
#' @export
#' @examples
#' compute_daily_steps(simulated_nhanes_steps,
#'                     group_cols = nhanes_group_cols())
compute_daily_steps = function(df,
                               mask_wear = FALSE,
                               group_cols = NULL) {
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  daily_steps_worker(x$df, x$group_cols)
}

#' @title Classify Steps/Day
#' @description Classifies steps/day, usually a person's mean across valid
#' days, into the step-volume categories of Tudor-Locke and Bassett (2004):
#'
#' | Category | Steps/day |
#' |---|---|
#' | `sedentary` | below 5,000 |
#' | `low_active` | 5,000 to 7,499 |
#' | `somewhat_active` | 7,500 to 9,999 |
#' | `active` | 10,000 to 12,499 |
#' | `highly_active` | 12,500 or more |
#' @param steps Numeric vector of steps/day.
#' @return A factor with the levels above, in order; `NA` where `steps` is
#' `NA`.
#' @references
#' Tudor-Locke C, Bassett DR Jr. How many steps/day are enough?
#' Preliminary pedometer indices for public health. *Sports Medicine*.
#' 2004;34(1):1-8. \doi{10.2165/00007256-200434010-00001}
#' @export
#' @examples
#' step_volume_category(c(3000, 5000, 9999, 10000, 15000, NA))
step_volume_category = function(steps) {
  assertthat::assert_that(
    is.numeric(steps),
    all(steps >= 0, na.rm = TRUE),
    msg = "`steps` must be numeric and non-negative."
  )
  cut(steps,
      breaks = c(0, 5000, 7500, 10000, 12500, Inf),
      labels = c("sedentary", "low_active", "somewhat_active", "active",
                 "highly_active"),
      include.lowest = TRUE,
      right = FALSE)
}
