group_string_cols = function(df, columns) {
  df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(columns))
    )
}
