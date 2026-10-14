validate_steps_data = function(df,
                               columns = c("time", "steps"),
                               check_minute = FALSE,
                               group_cols = NULL) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  missing_columns = setdiff(columns, names(df))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:",
               paste(missing_columns, collapse = ", ")))
  }


  if ("steps" %in% columns) {
    assertthat::assert_that(
      is.numeric(df$steps),
      all(df$steps >= 0, na.rm = TRUE),
      all(is.finite(df$steps) | is.na(df$steps)),
      msg = "Steps must be a numeric vector with finite, non-negative values."
    )
  }

  if ("time" %in% columns) {
    assertthat::assert_that(
      is.numeric(df$time) | is.integer(df$time) | lubridate::is.POSIXt(df$time),
      msg = "`time` must be numeric (minutes) or a date-time (POSIXct)."
    )

    if (anyNA(df$time)) {
      stop("Time values must not contain NA.")
    }

    if (length(group_cols) > 0) {
      time = NULL
      rm(list = c("time"))
      df = df %>%
        dplyr::ungroup() %>%
        dplyr::group_by(
          dplyr::across(dplyr::all_of(group_cols))
        ) %>%
        dplyr::mutate(
          time_diff = c(NA, diff(time))
        ) %>%
        dplyr::ungroup()
      time_diff = stats::na.omit(df$time_diff)
    } else {
      time_diff = diff(df$time)
    }
    group_hint = paste0(
      " If `df` contains more than one participant or day, set `group_cols`",
      " (or group `df` with dplyr::group_by()) to the columns identifying them."
    )
    assertthat::assert_that(
      all(time_diff >= 0),
      msg = paste0("Time values must be sorted (non-decreasing) ",
                   "within each group.", group_hint)
    )

    assertthat::assert_that(
      all(abs(time_diff - time_diff[1]) < sqrt(.Machine$double.eps)),
      msg = paste0("Time values must be equally spaced within each group, ",
                   "with no missing minutes.", group_hint)
    )

    if (check_minute) {
      if (lubridate::is.POSIXt(df$time)) {
        assertthat::assert_that(
          all(time_diff == lubridate::period(1, "minutes")) ||
            all(time_diff == 1L),
          msg = "Time values must be in 1-minute intervals."
        )
      } else {
        assertthat::assert_that(
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

  return(TRUE)
}

check_wear = function(wear) {
  ok = is.logical(wear) ||
    (is.numeric(wear) && all(wear %in% c(0, 1) | is.na(wear)))
  if (!ok) {
    stop("`wear` must be logical (TRUE = wear) or coded 0/1 (1 = wear).",
         call. = FALSE)
  }
  invisible(TRUE)
}

mask_nonwear_steps = function(df) {
  wear = steps = NULL
  rm(list = c("wear", "steps"))
  if (!assertthat::has_name(df, "wear")) {
    stop("`mask_wear = TRUE` requires a `wear` column in `df`.",
         call. = FALSE)
  }
  check_wear(df$wear)
  df = df %>%
    dplyr::mutate(
      wear = wear > 0,
      steps = dplyr::if_else(wear, as.numeric(steps), NA_real_)
    )
  df
}
