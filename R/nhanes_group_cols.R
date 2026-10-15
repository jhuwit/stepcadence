#' Group Columns for NHANES
#'
#' The columns that identify one participant-day in NHANES minute-level
#' accelerometer data: `SEQN` (respondent sequence number) and `PAXDAYM`
#' (day of wear).  Pass to `group_cols` to compute metrics per
#' participant-day.
#'
#' @returns The character vector `c("SEQN", "PAXDAYM")`.
#' @export
#' @examples
#' nhanes_group_cols()
#' compute_cadence_peak(simulated_nhanes_steps,
#'                      group_cols = nhanes_group_cols())
nhanes_group_cols <- function() {
  c("SEQN", "PAXDAYM")
}
