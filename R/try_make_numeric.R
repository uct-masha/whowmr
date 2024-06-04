#' Try to convert columns to numeric, replacing any na values with NA
#'
#' @param df the `data.frame` to use
#' @param cols the columns to convert to numeric, either as a vector of column names or a vector of column numbers
#' @param na the values to replace with NA, default is c("-", ".", "")
#' @param str_remove a regular expression to remove from the strings after replacing with NA values but before converting to numeric
#' @param warn if TRUE (the default), then a warning is issued if a column cannot be converted to numeric
#' @param stopAtFirstError if TRUE (the default), then the function stops at the first error, otherwise it continues
#'
#' @return the `data.frame` with the columns converted to numeric
try_make_numeric <- function(df, cols, na=c("-", ".", ""), str_remove=NULL, warn=TRUE, stopAtFirstError=TRUE) {
  badCols <- character(0)
  if (is.numeric(cols)) {
    if (min(cols)<0 | max(cols)>ncol(df)) {
      stop("cols must be numbers between 1 and the number of columns in df")
    }
    cols <- colnames(df)[cols]
  }
  for (i in seq_along(cols)) {
    .x <- df[,cols[i]]
    # Tibbles return a tibble with one column, so we extract that column
    # If the length was 1 per chance, then no harm done
    if (length(.x)==1) .x <- .x[[1]]
    # Now we need to convert the column to numeric
    # If it's already numeric then there's nothing to be done
    if (is.numeric(.x)) {
      next
    }
    # First trim the string
    .x <- gsub("^\\s+|\\s+$", "", .x)
    # Now we should remove and strings matching str_remove
    if (!is.null(str_remove)) {
      .x <- gsub(str_remove, "", .x)
    }
    # Replace any na values with NA
    for (naVal in na) {
      .x[.x==naVal] <- NA
    }
    # Now say which strings are numeric
    looksNumeric <- is.na(.x) | grepl(pattern="^-?\\d+\\.?\\d*$", .x)
    # If they all are then we can safely convert, otherwise we stop further processing
    if (any(!looksNumeric)) {
      badCols <- c(badCols, cols[i])
      if (isTRUE(warn)) {
        badIndices <- which(!looksNumeric)
        if (length(badIndices) > 5) {
          badIndicesSmall <- c(head(badIndices,3), tail(badIndices,2))
          badIndicesChar <- c(head(badIndices,3), "...", tail(badIndices,2))
        } else {
          badIndicesSmall <- badIndices
          badIndicesChar <- as.character(badIndices)
        }
        warning(paste0("Could not convert to numeric '",
                       cols[i],
                       "' at indices: ",
                       paste(badIndicesChar, collapse=", "),
                       " with values: '",
                       paste(.x[badIndicesSmall], collapse="', '"),
                       "'"
        ))
      }
      if (isTRUE(stopAtFirstError)) {
        stop("Stopping at first error")
      }
    }
    df[,cols[i]] <- as.numeric(.x)
  }
  if (length(badCols)>0) {
    stop("Unable to properly convert all columns to numeric. Bad columns: ", paste(badCols, collapse=", "))
  }
  df
}
