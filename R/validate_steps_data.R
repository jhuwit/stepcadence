validate_steps_data = function(df, columns = c("time", "steps", "walking", "ac"),
                         check_minute = FALSE) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  missing_columns = setdiff(columns, names(df))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  assertthat::assert_that(
    is.numeric(df$steps),
    msg = "Steps must be a numeric vector with non-negative values."
  )

  assertthat::assert_that(
    is.numeric(df$time) | is.integer(df$time) | lubridate::is.POSIXt(df$time)
  )

  if (any(is.na(df$time))) {
    stop("Time values must not contain NA.")
  }

  d = diff(time)
  assertthat::assert_that(
    all(d >= 0),
    msg = "Time values must be non-decreasing."
  )

  assertthat::assert_that(
    all(d == d[1]),
    msg = "Time values must be equally spaced."
  )

  if (check_minute) {
    if (lubridate::is.POSIXt(df$time)) {
      time_diff = diff(df$time)
      assertthat::assert_that(
        all(time_diff == lubridate::period(1, "minutes")),
        msg = "Time values must be in 1-minute intervals."
      )
    } else {
      assertthat::assert_that(
        all(diff(df$time) == 1),
        msg = "Time values must be in 1-minute intervals."
      )
    }
  }


  return(TRUE)
}
