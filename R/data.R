#' @importFrom tibble tibble
NULL

#' World Health Organization World Malaria Report 2018
#'
#' Annex data from the World Health Organization World Malaria Report 2018
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2018a}{p98: Policy adoption, 2017. Tanzania dropped.}
#'   \item{wmr2018b}{p102: Antimalarial drug policy, 2017.}
#'   \item{wmr2018c}{p104: Funding for malaria control, 2015–2017.}
#'   \item{wmr2018d}{p116: Commodities distribution and coverage, 2015–2017.}
#'   \item{wmr2018e}{p122: Household survey results, 2015–2017. No WHO Region column.}
#'   \item{wmr2018f}{p124: Population at risk and estimated malaria cases and deaths, 2010–2017.}
#'   \item{wmr2018g}{p138: Population at risk and reported malaria cases by place of care, 2017. Be careful not to double count Tanzania which is split into Mainland and Zanzibar.}
#'   \item{wmr2018h}{p142: Reported malaria cases by method of confirmation, 2010–2017}
#'   \item{wmr2018i}{p154: Reported malaria cases by species, 2010–2017}
#'   \item{wmr2018j}{p164: Reported malaria deaths, 2010–2017}
#' }
#' @source Report: \url{https://iris.who.int/bitstream/handle/10665/275867/9789241565653-eng.pdf?sequence=1}
#' @source Annex: \url{https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/wmr2018-excel-annexes.zip}
"wmr2018"

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
