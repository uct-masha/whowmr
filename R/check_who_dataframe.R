#' Check a dataframes dimensions and values to ensure it matches the underlying report
#'
#' @param df a dataframe
#' @param rows the number of rows expected in the dataframe
#' @param cols the number of columns expected in the dataframe
#' @param unique_values a named vector with names corresponding to columns in df
#'                      and values corresponding to the number of unique values
#'                      expected in each column. Used for the `WHO region` and
#'                      `Country/area` columns.
#' @param na_values a list with tuples corresponding to cells in df
#' @param known_values a list with tuples corresponding to cells in df and their known values
check_who_dataframe <- function(df,
                                rows,
                                cols,
                                unique_values,
                                na_values=list(),
                                known_values=list()) {
  # Check the number of rows
  checkmate::assert_true(nrow(df) == rows)
  # Check the number of columns
  checkmate::assert_true(ncol(df) == cols)
  # unique_values should be a named vector with names corresponding to columns in df
  checkmate::assert_vector(unique_values, names = "named", unique = TRUE)
  checkmate::assert_subset(names(unique_values), colnames(df))
  for (col in names(unique_values)) {
    checkmate::assert_true(length(unique(df[[col]])) == unique_values[[col]])
  }
  # na_values should be a list with tuples corresponding to cells in df
  # The first element in each tuple corresponds to the row and the second to the column
  checkmate::assert_list(na_values)
  for (na in na_values) {
    checkmate::assert_true(is.na(df[na[[1]], na[[2]], drop=T]))
  }
  # known_values should be a list with tuples corresponding to cells in df and their known values
  # The first element in each tuple corresponds to the row and the second to the column
  # The third element is the known value
  checkmate::assert_list(known_values)
  for (kv in known_values) {
    checkmate::assert_true(df[kv[[1]], kv[[2]], drop=T] == kv[[3]])
  }
}
