#' @keywords internal
#' @details
#' Start with [summarize_cadence()], which computes every metric.  The
#' individual metrics are available from [compute_daily_steps()],
#' [compute_cadence_peak()], [compute_cadence_max()],
#' [compute_cadence_mean()], [compute_cadence_level()],
#' [compute_cadence_minutes_above()], [compute_cadence_bouts()],
#' [compute_cadence_extended_bout()], and [compute_cadence_bands()].
#' [cadence_thresholds()] gives age-specific intensity thresholds and
#' [step_volume_category()] classifies steps/day.
#'
#' Input is minute-level data with `time` and `steps` columns, plus
#' optional `wear` and `counts` columns.  Use `group_cols` (or
#' [dplyr::group_by()]) to compute metrics per participant or per day.
#' @references
#' Tudor-Locke C, Han H, Aguiar EJ, et al. How fast is fast enough? Walking
#' cadence (steps/min) as a practical estimate of intensity in adults: a
#' narrative review. *British Journal of Sports Medicine*. 2018;52:776-788.
#' \doi{10.1136/bjsports-2017-097628}
#'
#' Saint-Maurice PF, Troiano RP, Bassett DR Jr, et al. Association of daily
#' step count and step intensity with mortality among US adults. *JAMA*.
#' 2020;323(12):1151-1160. \doi{10.1001/jama.2020.1382}
#'
#' del Pozo Cruz B, Ahmadi MN, Lee IM, Stamatakis E. Prospective
#' associations of daily step counts and intensity with cancer and
#' cardiovascular disease incidence and mortality and all-cause mortality.
#' *JAMA Internal Medicine*. 2022;182(11):1139-1148.
#' \doi{10.1001/jamainternmed.2022.4000}
#'
#' Tudor-Locke C, Bassett DR Jr. How many steps/day are enough?
#' Preliminary pedometer indices for public health. *Sports Medicine*.
#' 2004;34(1):1-8. \doi{10.2165/00007256-200434010-00001}
#'
#' Granat M, Clarke C, Holdsworth R, Stansfield B, Dall P. Quantifying the
#' cadence of free-living walking using event-based analysis. *Gait &
#' Posture*. 2015;42(1):85-90. \doi{10.1016/j.gaitpost.2015.04.012}
#'
#' Tudor-Locke C, Schuna JM Jr, Han H, et al. Cadence (steps/min) and
#' intensity during ambulation in 6-20 year olds: the CADENCE-kids study.
#' *International Journal of Behavioral Nutrition and Physical Activity*.
#' 2018;15:20. \doi{10.1186/s12966-018-0651-y}
#'
#' Tudor-Locke C, Aguiar EJ, Han H, et al. Walking cadence (steps/min) and
#' intensity in 21-40 year olds: CADENCE-adults.  *International Journal of
#' Behavioral Nutrition and Physical Activity*. 2019;16:8.
#' \doi{10.1186/s12966-019-0769-6}
#'
#' Tudor-Locke C, Ducharme SW, Aguiar EJ, et al. Walking cadence
#' (steps/min) and intensity in 41 to 60-year-old adults: the
#' CADENCE-adults study.  *International Journal of Behavioral Nutrition and
#' Physical Activity*. 2020;17:137. \doi{10.1186/s12966-020-01045-z}
#'
#' Tudor-Locke C, Mora-Gonzalez J, Ducharme SW, et al. Walking cadence
#' (steps/min) and intensity in 61-85-year-old adults: the CADENCE-Adults
#' study.  *International Journal of Behavioral Nutrition and Physical
#' Activity*. 2021;18:129. \doi{10.1186/s12966-021-01199-4}
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL
