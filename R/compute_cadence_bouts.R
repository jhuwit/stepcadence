# minutes that are in a bout, for one unit of minute-level steps
bout_mask = function(x, threshold, min_length, tolerance) {
  above = !is.na(x) & x >= threshold
  runs = rle(above)
  n_runs = length(runs$lengths)
  if (tolerance > 0 && n_runs > 2) {
    # a run below threshold that is not first or last lies between two runs
    # above threshold, so short ones are interruptions within a bout
    inner = seq_len(n_runs) %in% 2:(n_runs - 1)
    bridge = inner & !runs$values & runs$lengths <= tolerance
    runs$values[bridge] = TRUE
    runs = rle(inverse.rle(runs))
  }
  runs$values = runs$values & runs$lengths >= min_length
  list(in_bout = inverse.rle(runs), n_bouts = sum(runs$values))
}

bout_stats = function(x, threshold, min_length, tolerance) {
  b = bout_mask(x, threshold, min_length, tolerance)
  data.frame(n_bouts = b$n_bouts,
             bout_minutes = sum(b$in_bout),
             mean_bout_cadence = safe_mean(x[b$in_bout]))
}

bouts_worker = function(df, group_cols, threshold, min_length, tolerance) {
  steps = NULL
  rm(list = c("steps"))
  df |>
    group_string_cols(group_cols) |>
    dplyr::summarise(
      bout_stats(steps, threshold = threshold, min_length = min_length,
                 tolerance = tolerance),
      .groups = "drop"
    )
}

check_bout_args = function(threshold, min_length, tolerance) {
  assertthat::assert_that(
    is.numeric(threshold), length(threshold) == 1L, is.finite(threshold),
    threshold > 0,
    msg = "`bout_threshold` must be a single positive number."
  )
  assertthat::assert_that(
    assertthat::is.count(min_length),
    msg = "`bout_min_length` must be a single positive whole number."
  )
  assertthat::assert_that(
    is.numeric(tolerance), length(tolerance) == 1L, is.finite(tolerance),
    tolerance >= 0, tolerance == round(tolerance),
    msg = "`bout_tolerance` must be a single non-negative whole number."
  )
  invisible(TRUE)
}

#' @title Compute Cadence Bouts
#' @description Counts sustained bouts of stepping at or above a cadence
#' threshold, such as moderate-to-vigorous intensity (100 steps/min in
#' adults; see [cadence_thresholds()]), and the minutes spent in them.
#'
#' A bout is a run of consecutive minutes at or above `bout_threshold`
#' lasting at least `bout_min_length` minutes.  If `bout_tolerance` is
#' above 0, interruptions of up to `bout_tolerance` consecutive minutes
#' below the threshold do not end a bout; they count toward its length,
#' but a bout always starts and ends with a minute at or above the
#' threshold.  Minutes with missing steps (including masked non-wear) are
#' treated as below the threshold.
#'
#' Bout rules vary across studies; the defaults (10-minute bouts with no
#' interruptions) are a common, strict choice.  Accelerometer-count MVPA
#' bouts in NHANES (Troiano et al., 2008) allowed interruptions of 1-2
#' minutes, which `bout_tolerance = 2` approximates.
#' @inheritParams compute_cadence_peak
#' @param bout_threshold Cadence (steps/min) that each bout minute must
#' reach.
#' @param bout_min_length Minimum bout length in minutes.
#' @param bout_tolerance Longest interruption, in consecutive minutes below
#' `bout_threshold`, allowed within a bout.
#' @return A tibble with one row per group, the `group_cols`, and:
#' * `n_bouts`: number of bouts.
#' * `bout_minutes`: total minutes in bouts, including interruptions.
#' * `mean_bout_cadence`: mean steps/min over all minutes in bouts; `NA` if
#'   there are no bouts.
#'
#' Saint-Maurice et al. (2020) defined *extended bout cadence* as the mean
#' cadence over bouts of 2 or more consecutive minutes at 60 or more
#' steps/min: `compute_cadence_bouts(df, bout_threshold = 60,
#' bout_min_length = 2)`; see [compute_cadence_extended_bout()].
#' @references
#' Granat M, Clarke C, Holdsworth R, Stansfield B, Dall P. Quantifying the
#' cadence of free-living walking using event-based analysis. *Gait &
#' Posture*. 2015;42(1):85-90. \doi{10.1016/j.gaitpost.2015.04.012}
#'
#' Saint-Maurice PF, Troiano RP, Bassett DR Jr, et al. Association of daily
#' step count and step intensity with mortality among US adults. *JAMA*.
#' 2020;323(12):1151-1160. \doi{10.1001/jama.2020.1382}
#'

#' Troiano RP, Berrigan D, Dodd KW, et al. Physical activity in the United
#' States measured by accelerometer. *Medicine & Science in Sports &
#' Exercise*. 2008;40(1):181-188. \doi{10.1249/mss.0b013e31815a51b3}
#' @export
#' @examples
#' compute_cadence_bouts(simulated_nhanes_steps,
#'                       group_cols = nhanes_group_cols())
#'
#' # 12 minutes of brisk walking with a 1-minute pause
#' walk = data.frame(time = 1:14,
#'                   steps = c(0, rep(110, 6), 50, rep(110, 5), 0))
#' compute_cadence_bouts(walk)
#' compute_cadence_bouts(walk, bout_tolerance = 1)
compute_cadence_bouts = function(df,
                                 mask_wear = FALSE,
                                 bout_threshold = 100,
                                 bout_min_length = 10,
                                 bout_tolerance = 0,
                                 group_cols = NULL) {
  check_bout_args(bout_threshold, bout_min_length, bout_tolerance)
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  bouts_worker(x$df, x$group_cols, threshold = bout_threshold,
               min_length = bout_min_length, tolerance = bout_tolerance)
}


#' @title Compute Extended Bout Cadence
#' @description Computes extended bout cadence (Saint-Maurice et al.,
#' 2020): the mean cadence over all bouts of 2 or more consecutive minutes
#' at 60 or more steps/min.  This is [compute_cadence_bouts()] with
#' `bout_threshold = 60`, `bout_min_length = 2`, and `bout_tolerance = 0`.
#' @inheritParams compute_cadence_peak
#' @return A tibble with one row per group, the `group_cols`, and:
#' * `n_extended_bouts`: number of extended bouts.
#' * `extended_bout_cadence`: mean steps/min over those bouts; `NA` if there
#'   are none.  Saint-Maurice et al. (2020) analyzed people with no bouts as
#'   a separate group.
#' @references
#' Saint-Maurice PF, Troiano RP, Bassett DR Jr, et al. Association of daily
#' step count and step intensity with mortality among US adults. *JAMA*.
#' 2020;323(12):1151-1160. \doi{10.1001/jama.2020.1382}
#' @export
#' @examples
#' compute_cadence_extended_bout(simulated_nhanes_steps,
#'                               group_cols = nhanes_group_cols())
compute_cadence_extended_bout = function(df,
                                         mask_wear = FALSE,
                                         group_cols = NULL) {
  x = prepare_steps_data(df, mask_wear = mask_wear, group_cols = group_cols)
  extended_bout_worker(x$df, x$group_cols)
}

extended_bout_worker = function(df, group_cols) {
  n_bouts = mean_bout_cadence = NULL
  rm(list = c("n_bouts", "mean_bout_cadence"))
  bouts_worker(df, group_cols, threshold = 60, min_length = 2,
               tolerance = 0) |>
    dplyr::select(-"bout_minutes") |>
    dplyr::rename(n_extended_bouts = n_bouts,
                  extended_bout_cadence = mean_bout_cadence)
}
