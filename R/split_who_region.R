#' Split the contents of `WHO region\\nCountry/area` column in an annex sheet.
#'
#' @param df the dataframe from the excel sheet in the WMR annexes
#' @param col_region_area the number/name for the column containing the `WHO region\\nCountry/area` column
#' @param region_name the name of the new region column pulled out from the `WHO region\\nCountry/area` column
#' @param area_name the name of the new country/area column pulled out from the `WHO region\\nCountry/area` column
#' @param fill_down_first if TRUE (the default), fill down the region/area column before splitting the region and area names.
#'
#' I highly recommend using the `fill_down_first` option.
#'
#' @return the dataframe with the `col_region_area` column split into `region_name` and `area_name` columns.
#'
#' @examples
#' wmr2017a <- readxl::read_excel("wmr2017-excel-annexes/wmr2017-annex-table-a.xls", range = "A2:Q102")
#' split_who_region(wmr2017a, col_region_area=1, region_name="WHO Region", area_name="Country/area")
split_who_region <- function(df,
                             col_region_area=1,
                             region_name="WHO Region",
                             area_name="Country/area",
                             fill_down_first=TRUE) {
  if (length(intersect(c(region_name, area_name), colnames(df)))!=0) {
    stop("region_name and area_name must not be in the dataframe")
    # cli::cli_abort(c("x"="region_name ({region_name}) and area_name ({area_name}) must not be in the dataframe"))
  }
  # We need to ensure that the first col_region_area is well defined and passes
  # the is_new_region test
  if (is.na(df[1, col_region_area, drop=T])) {
    stop("First row of col_region_area must not be NA")
  }
  if (grepl("[a-z]", df[1, col_region_area, drop=TRUE])) {
    stop("First row of col_region_area must not contain lowercase letters")
  }
  df[,region_name] <- NA
  df[,area_name] <- NA
  for (i in seq(1, nrow(df))) {
    if (fill_down_first) {
      if (is.na(df[i, col_region_area])) {
        df[i, col_region_area] <- df[i-1, col_region_area]
      }
    }
    # The column represents a new region if it does not contain any lowercase letters
    # Note: This would technically include things like empty rows
    is_new_region <- !grepl("[a-z]", df[i, col_region_area, drop=TRUE])
    if (is_new_region) {
      # print(paste0("Updating ",col_region_area," because is.na(df[",i,",",col_na_when_region,"])"))
      # Setup a new value for the WHO Region column
      current_region <- df[i, col_region_area, drop=TRUE]
    } else {
      df[i, area_name] <- df[i, col_region_area, drop=TRUE]
    }
    df[i, region_name] <- current_region
  }
  rows_na_area <- is.na(df[,area_name, drop=TRUE])
  cnames <- setdiff(colnames(df), colnames(df[,col_region_area]))
  cnames_ordered <- c(region_name, area_name, head(cnames,-2))
  df <- df[!rows_na_area, cnames_ordered]
  df
}
