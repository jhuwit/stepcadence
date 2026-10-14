#' @keywords internal
#' @details
#' Start with [summarize_cadence()], which computes every metric.  The
#' individual metrics are available from [compute_cadence_peak()],
#' [compute_cadence_max()], [compute_cadence_mean()],
#' [compute_cadence_level()], and [compute_cadence_bands()].
#'
#' Input is minute-level data with `time` and `steps` columns, plus
#' optional `wear` and `counts` columns.  Use `group_cols` (or
#' [dplyr::group_by()]) to compute metrics per participant or per day.
#' @references
#' Tudor-Locke C, Han H, Aguiar EJ, et al. How fast is fast enough? Walking
#' cadence (steps/min) as a practical estimate of intensity in adults: a
#' narrative review. *British Journal of Sports Medicine*. 2018;52:776-788.
#' \doi{10.1136/bjsports-2017-097628}
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL
