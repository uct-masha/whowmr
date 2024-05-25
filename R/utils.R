#' Split the contents of `WHO region\\nCountry/area` column in an annex sheet.
#'
#' @param df the dataframe from the excel sheet in the WMR annexes
#' @param col_region_area the number/name for the column containing the `WHO region\\nCountry/area` column
#' @param region_name the name of the new region column pulled out from the `WHO region\\nCountry/area` column
#' @param area_name the name of the new country/area column pulled out from the `WHO region\\nCountry/area` column
#' @param col_na_when_region number/name of a reference column to use which is NA iff the `WHO region\\nCountry/area` column is a region. If NA (the default), then the `WHO region\\nCountry/area` column is a region iff contains no lower case letters.
#' @param fill_down_first if TRUE (the default), fill down the region/area column before splitting the region and area names.
#'
#' I highly recommend setting `col_na_when_region` to NA and using the `fill_down_first` option.
#'
#' @return the dataframe with the `col_region_area` column split into `region_name` and `area_name` columns.
#' @export
#'
#' @examples
#' wmr2017a <- readxl::read_excel("wmr2017-excel-annexes/wmr2017-annex-table-a.xls", range = "A2:Q102")
#' split_who_region(wmr2017a, col_region_area=1, region_name="WHO Region", area_name="Country/area", col_na_when_region=2)
split_who_region <- function(df,
                             col_region_area=1,
                             region_name="WHO Region",
                             area_name="Country/area",
                             col_na_when_region=NA,
                             fill_down_first=TRUE) {
  if (length(intersect(c(region_name, area_name), colnames(df)))!=0) {
    browser()
    stop("region_name and area_name must not be in the dataframe")
  }
  df[,region_name] <- NA
  df[,area_name] <- NA
  for (i in seq(1, nrow(df))) {
    if (fill_down_first) {
      if (is.na(df[i, col_region_area])) {
        df[i, col_region_area] <- df[i-1, col_region_area]
      }
    }
    if(is.na(col_na_when_region)) {
      is_new_region <- !grepl("[a-z]", df[i, col_region_area, drop=TRUE])
    } else {
      is_new_region <- is.na(df[i, col_na_when_region, drop=TRUE])
    }
    if (is_new_region) {
      # print(paste0("Updating ",col_region_area," because is.na(df[",i,",",col_na_when_region,"])"))
      current_region <- df[i, col_region_area, drop=TRUE]
    } else {
      df[i,area_name] <- df[i, col_region_area, drop=TRUE]
    }
    df[i,region_name] <- current_region
  }
  rows_na_area <- is.na(df[,area_name, drop=TRUE])
  cnames <- setdiff(colnames(df), colnames(df[,col_region_area]))
  cnames_ordered <- c(region_name, area_name, head(cnames,-2))
  df <- df[!rows_na_area, cnames_ordered]
  df
}

# compare_splits <- function(df) {
#   df1 <- split_who_region(df,col_region_area=1, col_na_when_region=2)
#   df2 <- split_who_region(df,col_region_area=1, col_na_when_region=NA)
#   if(all(all.equal(df1,df2)==TRUE)) {
#     return(TRUE)
#   } else {
#     print("FAILED: compared when col_na_when_region=2 and col_na_when_region=NA:")
#     print(all.equal(df1,df2))
#     print(df1)
#     print(df2)
#     print(tail(df1))
#     print(tail(df2))
#   }
# }
# wmr2017a <- readxl::read_excel("wmr2017-excel-annexes/wmr2017-annex-table-a.xls", range = "A2:Q102")
# compare_splits(wmr2017a)
# wmr2017b <- readxl::read_excel("wmr2017-excel-annexes/wmr2017-annex-table-b.xls", range = "A2:F102")
# compare_splits(wmr2017b)
# wmr2017c <- readxl::read_excel("wmr2017-excel-annexes/wmr2017-annex-table-c.xls", range = "A2:O286")
# compare_splits(wmr2017c)


