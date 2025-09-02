#' @title Load and Preprocess Accelerometry Data
#' @description Reads in an accelerometry dataset and orders it by ID, day, and minute.
#' @param filepath The file path to the dataset.
#' @param id_col Column name for participant ID. Default: "SEQN".
#' @param day_col Column name for day of wear. Default: "PAXDAYM".
#' @param min_col Column name for minute number. Default: "min_num".
#' @param steps_col Column name for step count. Default: "steps".
#' @param cadence_col Column name for cadence. Default: cadence
#' @return A data.table with ordered accelerometry data.
#' @export
load_accelerometry_data <- function(filepath,
                                    id_col = "SEQN",
                                    day_col = "PAXDAYM",
                                    min_col = "min_num",
                                    steps_col = "steps",
                                    cadence_col = "cadence") {
  ..id_col = ..day_col = ..min_col = NULL
  rm(list = c("..id_col", "..day_col", "..min_col"))
  dt_long <- data.table::fread(filepath,
                               colClasses = list(SEQN = "character")) %>%
    data.table::as.data.table()

  data.table::setorder(dt_long, ..id_col, ..day_col, ..min_col)
  return(dt_long)
}
