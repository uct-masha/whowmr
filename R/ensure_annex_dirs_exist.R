# This script is responsible for ensuring raw data from online (WHO Annexes) is
# downloaded locally. This allows development to be done offline without needing
# to constantly use the WHO servers. The local copies are not stored in the
# package or repository however, since they are available from the WHO website.
# These functions are meant to be used in the wmr20xx.R scripts which ensure
# that the local copy of the data is processed and stored.

# Note: Data from 2017-2024 are about 7MB zipped and 14MB unzipped.

#' Ensure the unzipped annex exists for the given year(s)
#'
#' @param url: The URL to download the annex from
#' @param filename: The filename of the zip file to be saved in
#'                  data-raw/downloads and unzipped as a directory in
#'                  data-raw/unzipped
#' @param removeZip: Whether to remove the zip file after unzipping
#'
#' @return named list of files in the unzipped annex
ensure_annex_dir_exists <- function(url, filename, removeZip=FALSE) {
  fpathZip <- file.path('data-raw/downloads', filename)
  fpathUnzipped <- file.path('data-raw/unzipped', gsub(".zip", "", filename))
  # Ensure downloads and unzipped directories exist
  if (!dir.exists("data-raw/downloads")) {
    dir.create("data-raw/downloads")
  }
  if (!dir.exists("data-raw/unzipped")) {
    dir.create("data-raw/unzipped")
  }
  # Ensure the zip is downloaded if we need it
  if (!file.exists(fpathUnzipped)) {
    # If the URL no longer works or the user is offline then we want to alert
    # them and let it fail:
    if (!file.exists(fpathZip)) {
      if (grepl("2024", filename)) {
        download_and_zip_2024_annexes(fpathZip)
      } else {
        tryCatch({
          download.file(url, fpathZip)
        }, error = function(e) {
          stop(paste0("Failed to download ", url, " to ", fpathZip, ". Please check the URL and your internet connection."))
        })
      }
    }
    # We also unzip the annexes to data-raw/unzipped
    zip::unzip(zipfile = fpathZip, exdir = fpathUnzipped)
    if (removeZip & file.exists(fpathZip)) {
      file.remove(fpathZip)
    }
  }
  # Finally return the list of files in the unzipped annex
  fnames <- list.files(fpathUnzipped, recursive = T, full.names = F)
  result <- list(fpath=fnames)
  names(result) <- fpathUnzipped
  return(result)
}

#' Ensure the unzipped annex exists for the given year(s)
#'
#' @param years: A numeric vector of years to download and unzip the annexes for
#'
#' @return fileTree: A named list of all files in the unzipped annexes, with the
#'                 names of the list being the directory of the unzipped annex
#'                 and the values being the file paths relative to that path.
ensure_annex_dirs_exist <- function(years=2017:2024) {
  checkmate::assert_subset(years, 2017:2024, empty.ok = FALSE)
  filenames <- c("wmr2017-excel-annexes.zip",
                 "wmr2018-excel-annexes.zip",
                 "wmr2019-excel-annexes.zip",
                 "wmr-2020-excel-annexes.zip",
                 "wmr2021-excel-annexes.zip",
                 "wmr2022-excel-annexes.zip",
                 "wmr2023-excel-annexes.zip",
                 "wmr2024-excel-annexes.zip")

  filenamesFiltered <- filenames[grepl(paste0(years, collapse="|"), filenames)]

  urls <- paste0("https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/",
                 filenamesFiltered)

  fileTree <- seq_along(urls) |>
    sapply(function(i) ensure_annex_dir_exists(urls[i], filenamesFiltered[i]))

  # We expect a named list of character vectors back. The names are the base
  # directory for that years annexes and the values are the relative file paths.
  checkmate::assert_list(fileTree, len=length(years), types="character", any.missing=FALSE)

  fileTree
}

download_and_zip_2024_annexes <- function(fpathZip) {
  # 2024 is a special case since the WHO website points to another url which
  # contains each annex as a link rather than pointing to a zip file. We need
  # to download each of these files and zip them up ourselves.
  # To get the links to the files we open the url and look for links to annex_.*.xlsx
  # files. We then download each of these files and zip them up.

  # Get the html from the url
  library(httr)
  url <- "https://www.who.int/publications/m/item/annexes-world-malaria-report-2024"
  # Read the html and parse out the xlsx links
  htmlContent <- httr::GET(url) |> httr::content("text")
  # Find the links to the annexes which look like https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/wmr2024_annex_2.xlsx
  # But there are multiple matches so we need to extract all of them
  annexLinks <- stringr::str_extract_all(htmlContent, "(https://[^ ]+\\.xls[x]{0,1})")[[1]]
  # Download each of the annexes
  pathDownload <- fs::path_join(c(tempdir(), "wmr2024-annexes"))
  fs::dir_create(pathDownload)
  for (link in annexLinks) {
    # Get the filename
    fname <- fs::path_file(link)
    # Download the file
    download.file(link, file.path(pathDownload, fname), mode="wb")
  }
  # Zip up the files
  zip::zipr(fpathZip, pathDownload)
  # Remove the downloaded files
  fs::dir_delete(pathDownload)
}

# wmr-2020-excel-annexes seemed to break the pattern so I wanted to analyse what
# was happening there. I recorded my initial findings here:
# whatsUpWith2020 <- function() {
#   # Interesting about 2020 there are 2 folders. Using kdiff3 I confirmed that
#   # they are the same folders but differ in 1 file.
#   fnameBroken <- "data-raw/unzipped/wmr-2020-excel-annexes/wmr-2020-excel-annexes(2021-03-11)/Annex 3 - G. Population denominator for case incidence and mortality rate, and reported malaria cases by place of care, 2019.xlsx"
#   # This seems to be a different version of Annex 3-I:
#   fnameNew <-    "data-raw/unzipped/wmr-2020-excel-annexes/wmr-2020-excel-annexes(2021-03-11)/Annex 3 - I. Reported malaria cases by species, 2010–2019.xlsx"
#   # Load these
#   dfBroken <- readxl::read_excel(fnameBroken, skip=3)
#   dfNew <- readxl::read_excel(fnameNew, skip=3)
#   # Manually inspect
#   View(dfBroken)
#   # Looks like they differ only in cell N25 which just has a rogue ° character
#   # Confirm with:
#   all.equal(dfBroken[,1:12], dfNew)
# }
