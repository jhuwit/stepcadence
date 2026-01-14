validate_steps_data = function(df,
                               columns = c("time", "steps", "walking"),
                               check_minute = FALSE,
                               group_cols = NULL) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  missing_columns = setdiff(columns, names(df))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }


  if ("steps" %in% columns) {
    assertthat::assert_that(
      is.numeric(df$steps),
      all(df$steps >= 0, na.rm = TRUE),
      msg = "Steps must be a numeric vector with non-negative values."
    )
  }

  if ("time" %in% columns) {
    assertthat::assert_that(
      is.numeric(df$time) | is.integer(df$time) | lubridate::is.POSIXt(df$time)
    )

    if (anyNA(df$time)) {
      stop("Time values must not contain NA.")
    }

    if (!is.null(group_cols)) {
      time = NULL
      rm(list = c("time"))
      df = df %>%
        dplyr::group_by(
          dplyr::across(dplyr::all_of(group_cols))
        ) %>%
        dplyr::mutate(
          # NEED CHECK FOR 1 minute times
          time_diff = c(NA, diff(time))
        ) %>%
        dplyr::ungroup()
      time_diff = stats::na.omit(df$time_diff)
    } else {
      time_diff = diff(df$time)
    }
    assertthat::assert_that(
      all(time_diff >= 0),
      msg = "Time values must be non-decreasing."
    )

    assertthat::assert_that(
      #     all(d == d[1]), # use below instead of this
      all(abs(time_diff - time_diff[1]) < sqrt(.Machine$double.eps)),
      msg = "Time values must be equally spaced."
    )

    if (check_minute) {
      if (lubridate::is.POSIXt(df$time)) {
        assertthat::assert_that(
          all(time_diff == lubridate::period(1, "minutes")),
          msg = "Time values must be in 1-minute intervals."
        )
      } else {
        assertthat::assert_that(
          # all(time_diff == 1), # use below instead of this
          all(abs(time_diff - 1L) < sqrt(.Machine$double.eps)),
          msg = "Time values must be in 1-minute intervals."
        )
      }
    }
  } else {
    if (check_minute) {
      warning("check_minute is TRUE, but time was not a required column")
    }
  }

  # if (!data.table::is.data.table(df)) {
  #   df = df %>% data.table::setDT()
  # }
  return(TRUE)
}


mask_nonwear_steps = function(df) {
  wear = steps = NULL
  rm(list = c("wear", "steps"))
  if (!assertthat::has_name(df, "wear")) {
    return(df)
  }
  assertthat::assert_that(
    is.logical(df$wear) |
      all(stats::na.omit(unique(df$wear)) %in% c(0, 1))
  )
  df = df %>%
    dplyr::mutate(wear = wear > 0)
  df = df %>%
    dplyr::mutate(steps = dplyr::if_else(wear, steps, NA_real_))
  df
}
