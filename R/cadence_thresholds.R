# CADENCE thresholds (steps/min) for moderate and vigorous intensity
cadence_threshold_table = function() {
  data.frame(
    age_min = c(6, 9, 12, 15, 18, 21, 41, 61),
    age_max = c(8, 11, 14, 17, 20, 40, 60, 85),
    moderate = c(125, 115, 110, 105, 90, 100, 100, 100),
    vigorous = c(155, 130, 125, 125, 125, 130, 130, NA),
    study = c(rep("CADENCE-Kids", 5), "CADENCE-Adults 21-40",
              "CADENCE-Adults 41-60", "CADENCE-Adults 61-85")
  )
}

#' @title Age-Specific Cadence Intensity Thresholds
#' @description Heuristic cadence thresholds (steps/min) for moderate and
#' vigorous intensity from the CADENCE studies, for use as
#' `step_thresholds` in [compute_cadence_level()] and [summarize_cadence()].
#'
#' | Age | Moderate | Vigorous | Source |
#' |---|---|---|---|
#' | 6-8 | 125 | 155 | CADENCE-Kids |
#' | 9-11 | 115 | 130 | CADENCE-Kids |
#' | 12-14 | 110 | 125 | CADENCE-Kids |
#' | 15-17 | 105 | 125 | CADENCE-Kids |
#' | 18-20 | 90 | 125 | CADENCE-Kids |
#' | 21-40 | 100 | 130 | CADENCE-Adults |
#' | 41-60 | 100 | 130 | CADENCE-Adults |
#' | 61-85 | 100 | `NA` | CADENCE-Adults |
#'
#' Moderate is 3 METs and vigorous 6 METs for adults (ages 18 and over);
#' for ages 6-17, youth METs (4 and 6) are used.  No vigorous threshold was
#' set for ages 61-85, because few participants reached 6 METs, so it is
#' `NA`; with `step_thresholds = c(100, NA)`, `high_cadence` is `NA`.
#' @param age `NULL` (the default) to return the whole table, or a single
#' age in years (6 to 85).  Fractional ages are rounded down.
#' @return If `age` is `NULL`, a data frame with columns `age_min`,
#' `age_max`, `moderate`, `vigorous`, and `study`.  Otherwise, a named
#' numeric vector `c(moderate = , vigorous = )`.
#' @references
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
#' @export
#' @examples
#' cadence_thresholds()
#' cadence_thresholds(35)
#' compute_cadence_level(simulated_nhanes_steps,
#'                       step_thresholds = cadence_thresholds(35),
#'                       group_cols = nhanes_group_cols())
cadence_thresholds = function(age = NULL) {
  tab = cadence_threshold_table()
  if (is.null(age)) {
    return(tab)
  }
  assertthat::assert_that(
    is.numeric(age),
    length(age) == 1L,
    is.finite(age),
    age >= 6, age < 86,
    msg = "`age` must be a single number from 6 to 85."
  )
  row = tab[floor(age) >= tab$age_min & floor(age) <= tab$age_max, ]
  c(moderate = row$moderate, vigorous = row$vigorous)
}
