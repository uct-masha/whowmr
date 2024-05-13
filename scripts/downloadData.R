downloadIfNotExists <- function(url, filename) {
  if (!file.exists(file.path('wmr', filename))) {
    download.file(url, file.path('wmr', filename))
  }
  if (!dir.exists(gsub(".zip", "", filename))) {
    zip::unzip(zipfile = file.path('wmr', filename), exdir = gsub(".zip", "", filename))
  }
  fnames <- list.files(gsub(".zip", "", filename), recursive = T, full.names = T)
  gsub(paste0("^",gsub(".zip", "", filename),"/"), "", fnames)
}

filenames <- c("wmr2017-excel-annexes.zip",
               "wmr2018-excel-annexes.zip",
               "wmr2019-excel-annexes.zip",
               "wmr-2020-excel-annexes.zip",
               "wmr2021-excel-annexes.zip",
               "wmr2022-excel-annexes.zip",
               "wmr2023-excel-annexes.zip")

urls <- paste0("https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/",
               filenames)

fileTree <- seq_along(urls) |>
  sapply(function(i) downloadIfNotExists(urls[i], filenames[i])) |>
  setNames(urls |> basename())

whatsUpWith2020 <- function() {
  # Interesting about 2020 there are 2 folders. Using kdiff3 I confirmed that
  # they are the same folders but differ in 1 file.
  fnameBroken <- "wmr-2020-excel-annexes/wmr-2020-excel-annexes(2021-03-11)/Annex 3 - G. Population denominator for case incidence and mortality rate, and reported malaria cases by place of care, 2019.xlsx"
  # This seems to be a different version of Annex 3-I:
  fnameNew <-    "wmr-2020-excel-annexes/wmr-2020-excel-annexes(2021-03-11)/Annex 3 - I. Reported malaria cases by species, 2010–2019.xlsx"
  # Load these
  dfBroken <- readxl::read_excel(fnameBroken, skip=3)
  dfNew <- readxl::read_excel(fnameNew, skip=3)
  # Manually inspect
  View(dfBroken)
  # Looks like they differ only in cell N25 which just has a rogue ° character
  # Confirm with:
  all.equal(dfBroken[,1:12], dfNew)
}

print(fileTree)
