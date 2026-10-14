cadence_band_labels = function() {
  c("zero", "incidental", "sporadic", "purposeful", "slow_walk",
    "medium_walk", "brisk_walk", "faster_ambulation")
}

# Bands from Tudor-Locke et al. (2018), Table 1:
# 0, 1-19, 20-39, 40-59, 60-79, 80-99, 100-119, and 120+ steps/min.
# Any value > 0 counts as movement, so (0, 20) is incidental.
cut_cadence_bands = function(steps) {
  all_bands = cadence_band_labels()
  band = cut(
    steps,
    breaks = c(0, 20, 40, 60, 80, 100, 120, Inf),
    labels = all_bands[-1],
    right = FALSE
  )
  band = factor(band, levels = all_bands)
  band[!is.na(steps) & steps == 0] = "zero"
  band
}

#' @title Compute Cadence Bands
#' @description `compute_cadence_bands()` assigns each minute to a cadence band
#' (steps/min) following Tudor-Locke et al. (2018):
#'
#' | Band | Steps/min |
#' |---|---|
#' | `zero` | 0 (non-movement) |
#' | `incidental` | > 0 to < 20 (1-19) |
#' | `sporadic` | 20-39 |
#' | `purposeful` | 40-59 |
#' | `slow_walk` | 60-79 |
#' | `medium_walk` | 80-99 |
#' | `brisk_walk` | 100-119 |
#' | `faster_ambulation` | 120 or more |
#'
#' Non-integer step values are placed in the band whose range contains them
#' (e.g. 19.5 is `incidental`).
#'
#' `summarize_cadence_bands()` counts the minutes in each band.
#'
#' @param df For `compute_cadence_bands()`, a data frame with a numeric
#' `steps` column (and a `wear` column if `mask_wear = TRUE`); `time` is not
#' required.  For `summarize_cadence_bands()`, the output of
#' `compute_cadence_bands()`, i.e. a data frame with a `cadence_band` column.
#' @inheritParams compute_cadence_peak
#' @return
#' * `compute_cadence_bands()`: `df` with an added factor column
#'   `cadence_band`.  Any dplyr grouping of `df` is kept.  Minutes with
#'   missing steps (including masked non-wear) have `NA` bands.
#' * `summarize_cadence_bands()`: a tibble with one row per group, the
#'   `group_cols`, and one integer column per band giving the number of
#'   minutes in that band.  Minutes with an `NA` band are not counted.
#' @references
#' Tudor-Locke C, Han H, Aguiar EJ, et al. How fast is fast enough? Walking
#' cadence (steps/min) as a practical estimate of intensity in adults: a
#' narrative review. *British Journal of Sports Medicine*. 2018;52:776-788.
#' \doi{10.1136/bjsports-2017-097628}
#' @export
#' @examples
#' df = data.frame(steps = c(0, 1, 19, 20, 59, 60, 99, 100, 119, 120, 150))
#' compute_cadence_bands(df)
#'
#' bands = compute_cadence_bands(simulated_nhanes_steps)
#' summarize_cadence_bands(bands, group_cols = nhanes_group_cols())
compute_cadence_bands <- function(df,
                                  mask_wear = FALSE
                                  ) {
  steps = NULL
  rm(list = c("steps"))
  check_flag(mask_wear, "mask_wear")
  validate_steps_data(df = df,
                      columns = c("steps"),
                      check_minute = FALSE)
  if (mask_wear) {
    df = df %>%
      mask_nonwear_steps()
  }
  gv = dplyr::group_vars(df)
  df = df %>%
    # ungroup for speed
    dplyr::ungroup() %>%
    dplyr::mutate(
      cadence_band = cut_cadence_bands(steps)
    )

  if (length(gv) > 0) {
    # regroup
    df = df %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(gv))
      )
  }

  df
}

#' @rdname compute_cadence_bands
#' @export
summarize_cadence_bands = function(df,
                                   group_cols = NULL
                                   ) {
  cadence_band = NULL
  rm(list = c("cadence_band"))
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  assertthat::assert_that(
    assertthat::has_name(df, "cadence_band")
  )
  group_cols = resolve_group_cols(df, group_cols)
  validate_steps_data(df = df, columns = group_cols)
  df = dplyr::as_tibble(df)
  if (!is.factor(df$cadence_band)) {
    warning("Cadence bands is not a factor (should be), coercing")
    values = sort(unique(stats::na.omit(df$cadence_band)))
    df = df %>%
      dplyr::mutate(
        cadence_band = factor(cadence_band,
                              levels = union(cadence_band_labels(), values))
      )
  }

  count_by_level(df,
                 var = "cadence_band",
                 levels = levels(df$cadence_band),
                 group_cols = group_cols)
}

#' @rdname compute_cadence_bands
#' @export
summarise_cadence_bands = summarize_cadence_bands

#' @title Ensure All Cadence Bands Exist
#' @description Adds a column of zeros for any cadence band missing from a
#' summary.  [summarize_cadence_bands()] already returns every band, so this
#' is only needed for summaries built some other way.
#' @param band_summary A data frame with one column per cadence band.
#' @param all_bands Character vector of the band columns that should exist.
#' Defaults to the bands returned by [compute_cadence_bands()].
#' @return `band_summary` with a column of 0s added for each missing band.
#' @export
#' @examples
#' ensure_all_cadence_bands(data.frame(id = 1, zero = 1000, incidental = 20))
ensure_all_cadence_bands <- function(band_summary,
                                     all_bands = cadence_band_labels()) {
  for (band in all_bands) {
    if (!(band %in% names(band_summary))) {
      band_summary[[band]] <- 0
    }
  }
  return(band_summary)
}
