#' Download WHO World Malaria Report PDFs
#'
#' @param years which years to download pdfs
#' @param outdir the path to the directory where pdfs should be downloaded
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' download_whowmr_reports()
download_whowmr_reports <- function(years=2017:2024, outdir='.') {
  urls <- list(w2024='https://iris.who.int/bitstream/handle/10665/379751/9789240104440-eng.pdf?sequence=1',
               w2023='https://iris.who.int/bitstream/handle/10665/374472/9789240086173-eng.pdf?sequence=1',
               w2022='https://iris.who.int/bitstream/handle/10665/365169/9789240064898-eng.pdf?sequence=1',
               w2021='https://iris.who.int/bitstream/handle/10665/350147/9789240040496-eng.pdf?sequence=1',
               w2020='https://iris.who.int/bitstream/handle/10665/337660/9789240015791-eng.pdf?sequence=1',
               w2019='https://iris.who.int/bitstream/handle/10665/330011/9789241565721-eng.pdf?sequence=1',
               w2018='https://iris.who.int/bitstream/handle/10665/275867/9789241565653-eng.pdf?sequence=1',
               w2017='https://iris.who.int/bitstream/handle/10665/259492/9789241565523-eng.pdf?sequence=1')

  # Download the pdfs
  if (!grepl('/$', outdir)) outdir <- paste0(outdir,'/')
  # if (!dir.exists(outdir)) dir.create(outdir)
  for (fname in names(urls)) {
    url <- urls[[fname]]
    fpath <- paste0(outdir, fname, '.pdf')
    if (any(grepl(years,fname))&!file.exists(fpath)) {
      tryCatch({
        download.file(url, fpath, mode='wb')
      }, error = function(e) {
        stop(paste0("Failed to download ", url, " to ", fpath, ". Please check the URL and your internet connection."))
      })
    }
  }
}
