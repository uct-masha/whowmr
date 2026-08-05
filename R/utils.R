#' Stop if suggested packages are not installed
#'
#' @param pkgs a vector of package names to check if they are installed
#'
#' @return NULL
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


