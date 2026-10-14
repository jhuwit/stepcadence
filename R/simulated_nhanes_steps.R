#' Simulated NHANES Steps
#'
#' Simulated minute-level step counts in the layout of NHANES accelerometer
#' data: 3 participants, each with 2 days of 1440 minutes.  Every day has the
#' same pattern of mostly 0 steps with blocks of 50, 100, 120, and 200
#' steps/min, so metrics are easy to check by hand.  See
#' `data-raw/simulate_step_data.R` in the source package.
#'
#' @format
#' A `data.table`/`data.frame` with 8640 rows and columns:
#' \describe{
#'   \item{SEQN}{Participant ID (character)}
#'   \item{PAXDAYM}{Day indicator (1 = first day)}
#'   \item{time}{Minute of the day, 1 to 1440 (integer)}
#'   \item{steps}{Steps in that minute (numeric)}
#' }
"simulated_nhanes_steps"
