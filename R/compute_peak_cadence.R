
#' @title Compute Peak Cadence Metrics
#' @description Computes 1-minute, 30-minute, and 60-minute peak cadence metrics.
#' @param dt Data.table containing accelerometry data.
#' @param id_col Column name for participant ID. Default: "SEQN".
#' @param day_col Column name for day of wear. Default: "PAXDAYM".
#' @param steps_col Column name for step count. Default: "steps".
#' @return A data.table with peak cadence metrics added.
#' @export
compute_peak_cadence <- function(dt,
                                 id_col = "SEQN",
                                 day_col = "PAXDAYM",
                                 steps_col = "steps") {
  dt[, peak_1min := max(get(steps_col), na.rm = TRUE),
     by = .(get(id_col), get(day_col))]
  dt[, peak_30min := mean(sort(get(steps_col), decreasing = TRUE)[1:30],
                          na.rm = TRUE),
     by = .(get(id_col), get(day_col))]
  dt[, peak_60min := mean(sort(get(steps_col), decreasing = TRUE)[1:60],
                          na.rm = TRUE),
     by = .(get(id_col), get(day_col))]
  return(dt)
}
