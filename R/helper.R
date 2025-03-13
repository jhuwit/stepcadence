#' @title Load and Preprocess Accelerometry Data
#' @description Reads in an accelerometry dataset and orders it by ID, day, and minute.
#' @param filepath The file path to the dataset.
#' @param id_col Column name for participant ID. Default: "SEQN".
#' @param day_col Column name for day of wear. Default: "PAXDAYM".
#' @param min_col Column name for minute number. Default: "min_num".
#' @param steps_col Column name for step count. Default: "steps".
#' @return A data.table with ordered accelerometry data.
#' @export
load_accelerometry_data <- function(filepath,
                                    id_col = "SEQN",
                                    day_col = "PAXDAYM",
                                    min_col = "min_num",
                                    steps_col = "steps") {
  dt_long <- fread(filepath, colClasses = list(SEQN = "character")) %>%
    as.data.table()

  setorder(dt_long, ..id_col, ..day_col, ..min_col)
  return(dt_long)
}

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

#' @title Compute Cadence Bands
#' @description Categorizes steps per minute into cadence bands.
#' @param dt Data.table containing accelerometry data.
#' @param steps_col Column name for step count. Default: "steps".
#' @return A list containing the updated data.table and a summary of cadence bands.
#' @export
compute_cadence_bands <- function(dt, steps_col = "steps") {
  all_bands <- c(
    "Zero",
    "Incidental",
    "Sporadic",
    "Purposeful",
    "Slow Walk",
    "Medium Walk",
    "Brisk Walk",
    "Fast Walk",
    "Very Fast Walk",
    "All-out"
  )

  dt[, cadence_band := cut(
    get(steps_col),
    breaks = c(0, 1, 19, 29, 49, 59, 79, 89, 99, 119, Inf),
    labels = all_bands,
    include.lowest = TRUE
  )]

  band_summary <- data.table::dcast(
    dt[, .N, by = .(SEQN, PAXDAYM, cadence_band)],
    SEQN + PAXDAYM ~ cadence_band,
    value.var = "N",
    fill = 0)

  return(list(
    dt = dt,
    band_summary = band_summary,
    all_bands = all_bands
  ))
}

#' @title Ensure All Cadence Bands Exist
#' @description Ensures that all cadence bands appear in the output summary.
#' @param band_summary Data.table with cadence band counts.
#' @param all_bands Vector of all expected cadence bands.
#' @return A data.table with all cadence bands present.
#' @export
ensure_all_cadence_bands <- function(band_summary, all_bands) {
  for (band in all_bands) {
    if (!(band %in% names(band_summary))) {
      band_summary[[band]] <- 0
    }
  }
  return(band_summary)
}

#' @title Compute Maximum Cadence Over Periods
#' @description Computes the maximum cadence over rolling windows of 5, 20, 30, and 60 minutes.
#' @param dt Data.table containing accelerometry data.
#' @param id_col Column name for participant ID. Default: "SEQN".
#' @param day_col Column name for day of wear. Default: "PAXDAYM".
#' @param steps_col Column name for step count. Default: "steps".
#' @return A data.table with maximum cadence metrics added.
#' @export
compute_max_cadence <- function(dt,
                                id_col = "SEQN",
                                day_col = "PAXDAYM",
                                steps_col = "steps") {
  dt[, max_5min := max(frollmean(get(steps_col), 5, align = "center", fill = 0), na.rm = TRUE), by = .(get(id_col), get(day_col))]
  dt[, max_20min := max(frollmean(get(steps_col), 20, align = "center", fill = 0), na.rm = TRUE), by = .(get(id_col), get(day_col))]
  dt[, max_30min := max(frollmean(get(steps_col), 30, align = "center", fill = 0), na.rm = TRUE), by = .(get(id_col), get(day_col))]
  dt[, max_60min := max(frollmean(get(steps_col), 60, align = "center", fill = 0), na.rm = TRUE), by = .(get(id_col), get(day_col))]
  return(dt)
}
