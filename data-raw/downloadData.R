# This script is responsible for ensuring raw data from online (WHO Annexes) is
# downloaded locally. This allows development to be done offline without needing
# to constantly use the WHO servers. The local copies are not stored in the
# package or repository however, since they are available from the WHO website.
# Other scripts will ensure the local copy of the data is processed and stored.

# Note: Data from 2017-2023 are about 6MB zipped and 14MB unzipped.

library(checkmate) # assertions
library(readxl) # read_excel
library(zip) # unzip

ensureAnnexRawDataExists <- function(url, filename) {
  fpathZip <- file.path('data-raw/downloads', filename)
  fpathUnzipped <- file.path('data-raw/unzipped', gsub(".zip", "", filename))
  # Ensure downloads and unzipped directories exist
  if (!dir.exists("data-raw/downloads")) {
    dir.create("data-raw/downloads")
  }
  if (!dir.exists("data-raw/unzipped")) {
    dir.create("data-raw/unzipped")
  }
  # Ensure the zip is downloaded
  if (!file.exists(fpathZip)) {
    download.file(url, fpathZip)
  }
  # We also unzip the annexes to data-raw/unzipped
  if (!dir.exists(fpathUnzipped)) {
    zip::unzip(zipfile = fpathZip, exdir = fpathUnzipped)
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
ensureRawDataExists <- function(years=2017:2023) {
  checkmate::assert_subset(years, 2017:2023)
  filenames <- c("wmr2017-excel-annexes.zip",
                 "wmr2018-excel-annexes.zip",
                 "wmr2019-excel-annexes.zip",
                 "wmr-2020-excel-annexes.zip",
                 "wmr2021-excel-annexes.zip",
                 "wmr2022-excel-annexes.zip",
                 "wmr2023-excel-annexes.zip")

  filenamesFiltered <- filenames[grepl(paste0(years, collapse="|"), filenames)]

  urls <- paste0("https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/",
                 filenamesFiltered)

  fileTree <- seq_along(urls) |>
    sapply(function(i) ensureAnnexRawDataExists(urls[i], filenamesFiltered[i]), simplify=T)

  fileTree
}

# wmr-2020-excel-annexes seemed to break the pattern so I wanted to analyse what
# was happening there. I recorded my initial findings here:
whatsUpWith2020 <- function() {
  # Interesting about 2020 there are 2 folders. Using kdiff3 I confirmed that
  # they are the same folders but differ in 1 file.
  fnameBroken <- "data-raw/unzipped/wmr-2020-excel-annexes/wmr-2020-excel-annexes(2021-03-11)/Annex 3 - G. Population denominator for case incidence and mortality rate, and reported malaria cases by place of care, 2019.xlsx"
  # This seems to be a different version of Annex 3-I:
  fnameNew <-    "data-raw/unzipped/wmr-2020-excel-annexes/wmr-2020-excel-annexes(2021-03-11)/Annex 3 - I. Reported malaria cases by species, 2010–2019.xlsx"
  # Load these
  dfBroken <- readxl::read_excel(fnameBroken, skip=3)
  dfNew <- readxl::read_excel(fnameNew, skip=3)
  # Manually inspect
  View(dfBroken)
  # Looks like they differ only in cell N25 which just has a rogue ° character
  # Confirm with:
  all.equal(dfBroken[,1:12], dfNew)
}
