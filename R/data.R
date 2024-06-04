#' @importFrom tibble tibble
NULL

#' World Health Organization World Malaria Report 2023
#'
#' Annex data from the World Health Organization World Malaria Report 2023.
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2023_2}{p142: Number of ITNs distributed through campaigns in malaria endemic countries, 2020–2022.}
#'   \item{wmr2023a}{p162: Policy adoption, 2022. Tanzania dropped.}
#'   \item{wmr2023b}{p168: Antimalarial drug policy, 2022}
#'   \item{wmr2023c}{p180: Funding for malaria control, 2020–2022}
#'   \item{wmr2023d}{p186: Commodities distribution and coverage, 2020–2022}
#'   \item{wmr2023ea}{p190: Household survey results, 2018–2022, compiled through STATcompiler. Suggestion: use [STATCompiler](https://statcompiler.com) or [rdhs](https://cran.r-project.org/web/packages/rdhs/index.html) package instead.}
#'   \item{wmr2023eb}{p192: Household survey results, 2018–2022, compiled through WHO calculations}
#'   \item{wmr2023f}{p232: Population denominator for case incidence and mortality rate, and estimated malaria cases and deaths, 2000-2022}
#'   \item{wmr2023g}{p236: Population denominator for case incidence and mortality rate, and reported malaria cases by place of care, 2022. Be careful not to double count Tanzania which is split into Mainland and Zanzibar.}
#'   \item{wmr2023h}{p260: Reported malaria cases by method of confirmation, 2010–2022}
#'   \item{wmr2023i}{p280: Reported malaria cases by species, 2010–2022}
#'   \item{wmr2023j}{p283: Reported malaria deaths, 2010–2022}
#' }
#' @source Report: \url{https://iris.who.int/bitstream/handle/10665/374472/9789240086173-eng.pdf?sequence=1}
#' @source Annex: \url{https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/wmr2023-excel-annexes.zip}
"wmr2023"

#' World Health Organization World Malaria Report 2022
#'
#' Annex data from the World Health Organization World Malaria Report 2022.
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2022_2}{p150: Number of ITNs distributed through campaigns in malaria endemic countries, 2020–2022.}
#'   \item{wmr2022a}{p170: Policy adoption, 2021. Tanzania dropped.}
#'   \item{wmr2022b}{p174: Antimalarial drug policy, 2021}
#'   \item{wmr2022c}{p176: Funding for malaria control, 2019–2021}
#'   \item{wmr2022d}{p188: Commodities distribution and coverage, 2019–2021}
#'   \item{wmr2022ea}{p194: Household survey results, 2017–2021, compiled through STATcompiler. Suggestion: use [STATCompiler](https://statcompiler.com) or [rdhs](https://cran.r-project.org/web/packages/rdhs/index.html) package instead.}
#'   \item{wmr2022eb}{p198: Household survey results, 2017–2021, compiled through WHO calculations}
#'   \item{wmr2022f}{p200: Population denominator for case incidence and mortality rate, and estimated malaria cases and deaths, 2000-2021}
#'   \item{wmr2022g}{p240: Population denominator for case incidence and mortality rate, and reported malaria cases by place of care, 2021. Be careful not to double count Tanzania which is split into Mainland and Zanzibar.}
#'   \item{wmr2022h}{p244: Reported malaria cases by method of confirmation, 2010–2021}
#'   \item{wmr2022i}{p268: Reported malaria cases by species, 2010–2021}
#'   \item{wmr2022j}{p290: Reported malaria deaths, 2010–2021}
#' }
#' @source Report: \url{https://iris.who.int/bitstream/handle/10665/365169/9789240064898-eng.pdf?sequence=1}
#' @source Annex: \url{https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/wmr2022-excel-annexes.zip}
"wmr2022"

#' World Health Organization World Malaria Report 2021
#'
#' Annex data from the World Health Organization World Malaria Report 2021.
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2021a}{p162: Policy adoption, 2020. Tanzania dropped.}
#'   \item{wmr2021b}{p166: Antimalarial drug policy, 2020}
#'   \item{wmr2021c}{p168: Funding for malaria control, 2018–2020}
#'   \item{wmr2021d}{p180: Commodities distribution and coverage, 2018–2020}
#'   \item{wmr2021ea}{p186: Household survey results, 2016–2020, compiled through STATcompiler. Suggestion: use [STATCompiler](https://statcompiler.com) or [rdhs](https://cran.r-project.org/web/packages/rdhs/index.html) package instead.}
#'   \item{wmr2021eb}{p190: Household survey results, 2016–2020, compiled through WHO calculations}
#'   \item{wmr2021f}{p192: Population denominator for case incidence and mortality rate, and estimated malaria cases and deaths, 2000-2020}
#'   \item{wmr2021g}{p232: Population denominator for case incidence and mortality rate, and reported malaria cases by place of care, 2020. Be careful not to double count Tanzania which is split into Mainland and Zanzibar.}
#'   \item{wmr2021h}{p236: Reported malaria cases by method of confirmation, 2010–2020}
#'   \item{wmr2021i}{p248: Reported malaria cases by species, 2010–2020}
#'   \item{wmr2021j}{p260: Reported malaria deaths, 2010–2020}
#' }
#' @source Report: \url{https://iris.who.int/bitstream/handle/10665/350147/9789240040496-eng.pdf?sequence=1}
#' @source Annex: \url{https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/wmr2021-excel-annexes.zip}
"wmr2021"

#' World Health Organization World Malaria Report 2020
#'
#' Annex data from the World Health Organization World Malaria Report 2020.
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2020a}{p156: Policy adoption, 2019. Tanzania dropped.}
#'   \item{wmr2020b}{p160: Antimalarial drug policy, 2019}
#'   \item{wmr2020c}{p162: Funding for malaria control, 2017–2019}
#'   \item{wmr2020d}{p174: Commodities distribution and coverage, 2017–2019}
#'   \item{wmr2020ea}{p180: Household survey results, 2015–2019, compiled through STATcompiler. Suggestion: use [STATCompiler](https://statcompiler.com) or [rdhs](https://cran.r-project.org/web/packages/rdhs/index.html) package instead.}
#'   \item{wmr2020eb}{p184: Household survey results, 2015–2019, compiled through WHO calculations}
#'   \item{wmr2020f}{p186: Population denominator for case incidence and mortality rate, and estimated malaria cases and deaths, 2000-2019}
#'   \item{wmr2020g}{p216: Population denominator for case incidence and mortality rate, and reported malaria cases by place of care, 2019. Be careful not to double count Tanzania which is split into Mainland and Zanzibar.}
#'   \item{wmr2020h}{p220: Reported malaria cases by method of confirmation, 2010–2019}
#'   \item{wmr2020i}{p232: Reported malaria cases by species, 2010–2019}
#'   \item{wmr2020j}{p244: Reported malaria deaths, 2010–2019}
#' }
#' @source Report: \url{https://iris.who.int/bitstream/handle/10665/337660/9789240015791-eng.pdf?sequence=1}
#' @source Annex: \url{https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/wmr-2020-excel-annexes.zip}
"wmr2020"

#' World Health Organization World Malaria Report 2019
#'
#' Annex data from the World Health Organization World Malaria Report 2019.
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2019a}{p114: Policy adoption, 2018. Tanzania dropped.}
#'   \item{wmr2019b}{p118: Antimalarial drug policy, 2018}
#'   \item{wmr2019c}{p120: Funding for malaria control, 2016–2018}
#'   \item{wmr2019d}{p132: Commodities distribution and coverage, 2016–2018}
#'   \item{wmr2019ea}{p138: Household survey results, 2015–2018, compiled through STATcompiler. Suggestion: use [STATCompiler](https://statcompiler.com) or [rdhs](https://cran.r-project.org/web/packages/rdhs/index.html) package instead.}
#'   \item{wmr2019eb}{p140: Household survey results, 2015–2018, compiled through WHO calculations}
#'   \item{wmr2019f}{p142: Population at risk and estimated malaria cases and deaths, 2010–2018}
#'   \item{wmr2019g}{p158: Population at risk and reported malaria cases by place of care, 2018. Be careful not to double count Tanzania which is split into Mainland and Zanzibar.}
#'   \item{wmr2019h}{p162: Reported malaria cases by method of confirmation, 2010–2018}
#'   \item{wmr2019i}{p174: Reported malaria cases by species, 2010–2018}
#'   \item{wmr2019j}{p184: Reported malaria deaths, 2010–2018}
#' }
#' @source Report: \url{https://iris.who.int/bitstream/handle/10665/330011/9789241565721-eng.pdf?sequence=1}
#' @source Annex: \url{https://cdn.who.int/media/docs/default-source/malaria/world-malaria-reports/wmr2019-excel-annexes.zip}
"wmr2019"

#' World Health Organization World Malaria Report 2018
#'
#' Annex data from the World Health Organization World Malaria Report 2018.
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2018a}{p98: Policy adoption, 2017. Tanzania dropped.}
#'   \item{wmr2018b}{p102: Antimalarial drug policy, 2017.}
#'   \item{wmr2018c}{p104: Funding for malaria control, 2015–2017.}
#'   \item{wmr2018d}{p116: Commodities distribution and coverage, 2015–2017.}
#'   \item{wmr2018e}{p122: Household survey results, 2015–2017. No WHO Region column. Suggestion: use [STATCompiler](https://statcompiler.com) or [rdhs](https://cran.r-project.org/web/packages/rdhs/index.html) package instead.}
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
#' Annex data from the World Health Organization World Malaria Report 2017.
#' Note that Tanzania is typically split into Mainland and Zanzibar
#'
#' @format A list containing a tibble for each item in the annex:
#' \describe{
#'   \item{wmr2017a}{p97: Policy Adoption, 2016. Tanzania dropped.}
#'   \item{wmr2017b}{p98: Antimalarial drug policy, 2016.}
#'   \item{wmr2017c}{p100: Funding for malaria control, 2014–2016.}
#'   \item{wmr2017d}{p112: Commodities distribution and coverage, 2014–2016.}
#'   \item{wmr2017e}{p118: Household survey results, 2014–2016. Suggestion: use [STATCompiler](https://statcompiler.com) or [rdhs](https://cran.r-project.org/web/packages/rdhs/index.html) package instead.}
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
