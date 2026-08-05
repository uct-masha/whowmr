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
download_whowmr_reports <- function(years=2017:2025, outdir='.') {
  urls <- list(
    w2025 = 'https://iris.who.int/server/api/core/bitstreams/be20b8dc-cbfa-42a6-ad17-7679ceb75360/content',
    w2024 = 'https://iris.who.int/server/api/core/bitstreams/6cd14c66-a2d6-408e-8f4c-7dbed9c46ae8/content',
    w2023 = 'https://iris.who.int/server/api/core/bitstreams/5a85508d-27c0-4fab-97d0-08e0ab468ad8/content',
    w2022 = 'https://iris.who.int/server/api/core/bitstreams/e739203d-5747-46d0-bf6c-ca272978d20c/content',
    w2021 = 'https://iris.who.int/server/api/core/bitstreams/82894e5c-5343-49c9-82ca-d497c3890744/content',
    w2020 = 'https://iris.who.int/server/api/core/bitstreams/962919a2-6c47-41d8-83e7-ab7fe7812d69/content',
    w2019 = 'https://iris.who.int/server/api/core/bitstreams/40d72fab-aa6a-4dd8-af3f-4fc1d3a71981/content',
    w2018 = 'https://iris.who.int/server/api/core/bitstreams/c34e8c3c-8b10-498e-9414-bc68f3662889/content',
    w2017 = 'https://iris.who.int/server/api/core/bitstreams/b4ed5b7e-13db-4d05-8b81-0835c50ec276/content'
  )

  # Prepare output path
  if (!grepl("/$", outdir)) outdir <- paste0(outdir, "/")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  # Create regex pattern combining all requested years (e.g. "2017|2018|2019")
  years_pattern <- paste(years, collapse = "|")

  for (fname in names(urls)) {
    url <- urls[[fname]]
    fpath <- paste0(outdir, fname, '.pdf')

    if (grepl(years_pattern, fname) && !file.exists(fpath)) {
      tryCatch({
        download.file(url, fpath, mode = 'wb')
      }, error = function(e) {
        stop(paste0("Failed to download ", url, " to ", fpath, ". Please check the URL and your internet connection."))
      })
    }
  }
}
