#' @importFrom tibble tibble
NULL

#' World Health Organization World Malaria Report 2017
#'
#' Annex data from the World Health Organization World Malaria Report 2017
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2017a}{p97: Policy Adoption, 2016. Tanzania dropped.}
#'   \item{wmr2017b}{p98: Antimalarial drug policy, 2016.}
#'   \item{wmr2017c}{p100: Funding for malaria control, 2014–2016.}
#'   \item{wmr2017d}{p112: Commodities distribution and coverage, 2014–2016.}
#'   \item{wmr2017e}{p118: Household survey results, 2014–2016. No WHO Region column.}
#'   \item{wmr2017fa}{p120: Estimated malaria cases and deaths, 2010–2016. Some columns have, eg, "<1", and you should decide how to treat this.}
#'   \item{wmr2017fb}{p132: Population at risk for estimates of malaria cases and deaths, 2010–2016.}
#'   \item{wmr2017g}{p134: Population at risk and reported malaria cases by place of care, 2016. Be careful not to double count Tanzania which is split into Mainland and Zanzibar.}
#'   \item{wmr2017h}{p138: Reported malaria cases by method of confirmation, 2010–2016}
#'   \item{wmr2017i}{p150: Reported malaria cases by species, 2010–2016}
#'   \item{wmr2017j}{p158: Reported malaria deaths, 2010–2016}
#' }
#' @source Report: \url{https://iris.who.int/bitstream/handle/10665/259492/9789241565523-eng.pdf?sequence=1}
#' @source Annex: \url{https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/wmr2017-excel-annexes.zip}
"wmr2017"
