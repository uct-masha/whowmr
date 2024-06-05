#' Stop if suggested packages are not installed
#'
#' @param pkgs a vector of package names to check if they are installed
#'
#' @examples
#' stop_if_not_installed(c("readxl", "dplyr"))
#' stop_if_not_installed()
stop_if_not_installed <- function(...) {
  args <- list(...)
  # If pkgs was not supplied, get it from DESCRIPTION
  pkgs <- if (length(args)==0) {
    strsplit(read.dcf("DESCRIPTION")[,'Suggests'], "\n")[[1]]
  } else {
    args
  }
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package ", pkg, " is not installed. Please install it first."))
    }
  }
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


