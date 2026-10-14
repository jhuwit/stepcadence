group_string_cols = function(df, columns) {
  if (length(columns) > 0) {
    df = df %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(columns))
      )
  }
  df
}

# Use `group_cols` if given, otherwise any dplyr groups on `df`,
# otherwise no grouping (character(0))
resolve_group_cols = function(df, group_cols = NULL) {
  if (is.null(group_cols) && dplyr::is_grouped_df(df)) {
    group_cols = dplyr::group_vars(df)
  }
  if (is.null(group_cols)) {
    group_cols = character(0)
  }
  assertthat::assert_that(
    is.character(group_cols),
    !anyNA(group_cols),
    msg = "`group_cols` must be NULL or a character vector of column names."
  )
  unique(group_cols)
}

# Count, within groups, the number of rows in each level of `var`.
# Every level gets a column (0 if absent) and NA values are not counted,
# so no "NA" column is created and all-NA groups are kept.
count_by_level = function(df, var, levels, group_cols) {
  x = df[[var]]
  tmp_names = paste0(".level_", seq_along(levels))
  indicators = lapply(levels, function(lev) as.integer(x == lev))
  names(indicators) = tmp_names

  out = dplyr::select(df, dplyr::all_of(group_cols)) %>%
    dplyr::bind_cols(dplyr::as_tibble(indicators)) %>%
    group_string_cols(group_cols) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(tmp_names), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  names(out)[match(tmp_names, names(out))] = levels
  out
}

join_summaries = function(summaries, group_cols) {
  if (length(group_cols) == 0) {
    return(dplyr::bind_cols(summaries))
  }
  Reduce(function(x, y) dplyr::full_join(x, y, by = group_cols), summaries)
}

# mean that returns NA (not NaN) when there is no non-missing data
safe_mean = function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

check_flag = function(x, name) {
  if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

check_step_thresholds = function(step_thresholds) {
  assertthat::assert_that(
    is.numeric(step_thresholds),
    length(step_thresholds) == 2L,
    all(is.finite(step_thresholds)),
    all(step_thresholds > 0),
    length(unique(step_thresholds)) == 2L,
    msg = paste0(
      "`step_thresholds` must be two different, positive, ",
      "finite numbers, e.g. c(60, 120)."
    )
  )
  sort(step_thresholds)
}

check_counts_threshold = function(counts_threshold) {
  assertthat::assert_that(
    is.numeric(counts_threshold),
    length(counts_threshold) == 1L,
    is.finite(counts_threshold),
    counts_threshold >= 0,
    msg = "`counts_threshold` must be a single non-negative number."
  )
  invisible(TRUE)
}
