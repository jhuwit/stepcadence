
#' @title Calculate Cadence
#' @description Calculates cadence given steps, time, and optionally, walking flag.
#' When not walking, cadence is assigned NA.
#' @param df A `data.frame` of data, which must have the columns of
#' `time` and `steps`.
#' @param group_cols columns to group the data by before computing cadence metrics
#' @return A data.table with cadence added.
#' @export
calculate_cadence <- function(df,
) {
  # issues with no symbol in the environment
  steps = NULL
  rm(list = c("steps"))
  assertthat::assert_that(
    assertthat::has_name(df, "steps")
  )

  if (!"walking" %in% names(df)) {
    df$walking = rep(TRUE, nrow(df))
  }

  validate_steps_data(df = df,
                      columns = c("time", "steps", "walking"),
                      check_minute = TRUE)

  df = df %>%
    mutate(cadence = ifelse(walking, steps, NA))

  return(df)
}

## TODO: consolidate cadence v.s. steps for the remaining functions
