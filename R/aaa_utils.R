group_string_cols = function(df, columns) {
  if (!is.null(columns)) {
  df = df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(columns))
    )
  } else {
    df
  }
  df
}
