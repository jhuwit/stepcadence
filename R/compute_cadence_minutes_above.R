minutes_above_names = function(thresholds) {
  paste0("minutes_", vapply(thresholds, format, character(1),
                            scientific = FALSE), "plus")
}

minutes_above_worker = function(df, group_cols, thresholds) {
  out = df |>
    dplyr::select(dplyr::all_of(group_cols)) |>
    dplyr::bind_cols(
      stats::setNames(
        lapply(thresholds, function(t) as.integer(df$steps >= t)),
        minutes_above_names(thresholds)
      )
    ) |>
    group_string_cols(group_cols) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  out
}

check_minutes_above = function(thresholds) {
  assertthat::assert_that(
    is.numeric(thresholds),
    length(thresholds) >= 1L,
    all(is.finite(thresholds)),
    all(thresholds > 0),
    !anyDuplicated(thresholds),
    msg = "`thresholds` must be distinct, positive, finite numbers."
  )
  sort(thresholds)
}

#' @title Compute Minutes at or Above Cadence Thresholds
#' @description Counts the minutes at or above each cadence threshold.  The
#' defaults are 40 steps/min, the lower bound of purposeful steps (del Pozo
#' Cruz et al., 2022), and 100 steps/min, the adult moderate-intensity
#' threshold (Tudor-Locke et al., 2019; Paluch et al., 2021).  Unlike
#' [compute_cadence_level()], the counts are cumulative: a minute at 120
#' steps/min counts toward both `minutes_40plus` and `minutes_100plus`.
#' @inheritParams compute_cadence_peak
#' @param thresholds Distinct, positive numbers (steps/min).
#' @return A tibble with one row per group, the `group_cols`, and one
#' integer column per threshold, named `minutes_<threshold>plus` (e.g.
#' `minutes_40plus`), giving the number of minutes with steps at or above
#' it.  Minutes with missing steps are not counted.
#' @references
#' del Pozo Cruz B, Ahmadi MN, Lee IM, Stamatakis E. Prospective
#' associations of daily step counts and intensity with cancer and
#' cardiovascular disease incidence and mortality and all-cause mortality.
#' *JAMA Internal Medicine*. 2022;182(11):1139-1148.
#' \doi{10.1001/jamainternmed.2022.4000}
#'
#' Paluch AE, Gabriel KP, Fulton JE, et al. Steps per day and all-cause
#' mortality in middle-aged adults in the Coronary Artery Risk Development
#' in Young Adults study. *JAMA Network Open*. 2021;4(9):e2124516.
#' \doi{10.1001/jamanetworkopen.2021.24516}
#'
#' Tudor-Locke C, Aguiar EJ, Han H, et al. Walking cadence (steps/min) and
#' intensity in 21-40 year olds: CADENCE-adults.  *International Journal of
#' Behavioral Nutrition and Physical Activity*. 2019;16:8.
#' \doi{10.1186/s12966-019-0769-6}
#' @export
#' @examples
#' compute_cadence_minutes_above(simulated_nhanes_steps,
#'                               group_cols = nhanes_group_cols())
#' compute_cadence_minutes_above(simulated_nhanes_steps,
#'                               thresholds = c(60, 100, 130),
#'                               group_cols = nhanes_group_cols())
compute_cadence_minutes_above = function(df,
                                         mask_wear = FALSE,
                                         thresholds = c(40, 100),
                                         group_cols = NULL) {
  thresholds = check_minutes_above(thresholds)
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  minutes_above_worker(x$df, x$group_cols, thresholds)
}
