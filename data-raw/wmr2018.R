# The downloadData.R file exposes `ensureRawDataExists` which fetches raw data
# from the WHO website and unzips it to a local directory.
# This file brings the unzipped data from the 2018 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2018 page 67:
# Annex 1 - Data sources and methods
# Annex 2 - Regional profiles
# >> A. West Africa
# >> B. Central Africa
# >> C. East and Southern Africa
# >> D. Countries with low transmission in East and Southern Africa
# >> E. Region of the Americas
# >> F. Eastern Mediterranean Region
# >> G. European Region
# >> H. South-East Asia Region
# >> I. Western Pacific Region
# Annex 3 - Data tables
# >> A. Policy adoption, 2017
# >> B. Antimalarial drug policy, 2017
# >> C. Funding for malaria control, 2015–2017
# >> D. Commodities distribution and coverage, 2015–2017
# >> E. Household survey results, 2015–2017
# >> F. Population at risk and estimated malaria cases and deaths, 2010–2017
# >> G. Population at risk and reported malaria cases by place of care, 2017
# >> H. Reported malaria cases by method of confirmation, 2010–2017
# >> I. Reported malaria cases by species, 2010–2017
# >> J. Reported malaria deaths, 2010–2017

get_wmr2018 <- function() {
  # First we need to ensure the unzipped annex file exists:
  source('data-raw/downloadData.R')
  fileTree <- ensureRawDataExists(2018)
  basePath <- names(fileTree)[[1]]
  # TODO: Remove footnotes in column names
  # TODO: Remove footnotes in cell values

  # wmr2018a ####
  # wmr2017a ####
  to_T_F_NA <- Vectorize(function(x) {
    if(is.na(x)) return(NA)
    if(x%in%c("Y","●")) return(T)
    if(x%in%c("N","◌")) return(F)
    NA
  })
  wmr2018a <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-a.xls"), range = "A2:Q102") |>
    dplyr::mutate(`WHO region\nCountry/area` = dplyr::if_else(`WHO region\nCountry/area`=="Western Pacific",
                                                              toupper(`WHO region\nCountry/area`),
                                                              `WHO region\nCountry/area`)) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(dplyr::across(3:18, to_T_F_NA)) |>
    dplyr::filter(`Country/area`!="United Republic of Tanzania3")

  # wmr2018b ####
  wmr2018b <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-b.xls"), range = "A2:F102") |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::rename(`Uncomplicated unconfirmed`=`Uncomplicated\nunconfirmed`,
                  `Uncomplicated confirmed`=`Uncomplicated\nconfirmed`)

  # wmr2018c ####
  wmr2018c <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-c.xls"),
                                 range = "A6:O289",
                                 col_names = c("WHO region\nCountry/Area",
                                               "Year",
                                               "Donor_Global Fund",
                                               "Donor_PMI/USAID",
                                               "Donor_The World Bank",
                                               "Donor_UK",
                                               "Country_Governement (NMCP)",
                                               # column 8 holds footnotes on how to interpret column 7
                                               "Budget not expenditure",
                                               "Country_Global Fund",
                                               "Country_The World Bank",
                                               "Country_PMI/USAID",
                                               "Country_Other bilaterals",
                                               "Country_WHO",
                                               "Country_UNICEF",
                                               "Country_Other contributions")) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(
      # Translate the footnotes into a boolean column
      `Budget not expenditure`=!is.na(`Budget not expenditure`),
      # Some cells (eg Pakistan 2016) reported eg "16’400’000"
      # which won't parse well, so we clean that up before parsing:
      dplyr::across(setdiff(4:16,9),
                    ~as.numeric(stringr::str_replace_all(.x,"[’']+",""))))

  # wmr2018d ####
  wmr2018d <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-d.xls"), range = "A1:I294") |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    # column 7 is irs coverage and includes <1 so we impose no parsing to numeric there
    dplyr::mutate(dplyr::across(setdiff(4:10,7), ~as.numeric(stringr::str_replace_all(.x,pattern = "[-']",replacement = ""))))

  # wmr2018e ####
  wmr2018e <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-e.xls"),
                                 sheet="Sheet2", range="A8:U77",
                                 col_names = c("Country", "Source",
                                               "Households with at least one insecticide-treated mosquito net (ITN)",
                                               "Households with at least one insecticide-treated mosquito net (ITN) for every two persons who stayed in the household the previous night",
                                               "Households with indoor residual spraying (IRS) in last 12 months",
                                               "Households with at least one insecticide-treated mosquito net (ITN) and/or indoor residual spraying (IRS) in the past 12 months",
                                               "Households with at least one insecticide-treated mosquito net (ITN) for every two persons and/or indoor residual spraying (IRS) in the past 12 months",
                                               "Persons with access to an insecticide-treated mosquito net (ITN)",
                                               "Population who slept under an insecticide-treated mosquito net (ITN) last night",
                                               "Existing insecticide-treated mosquito nets (ITNs) used last night",
                                               "Children under 5 who slept under an insecticide-treated net (ITN)",
                                               "Pregnant women who slept under an insecticide-treated net (ITN)",
                                               "SP/Fansidar 3+ doses during pregnancy",
                                               "SP/Fansidar 3+ doses, at least one during ANC visit (IPTp)",
                                               "Children with fever for whom advice or treatment was sought",
                                               "Children with fever who had blood taken from a finger or heel for testing",
                                               "Children with fever who took antimalarial drugs",
                                               "Children who took any ACT",
                                               "Children with hemoglobin lower than 8.0 g/dl",
                                               "Malaria prevalence according to RDT",
                                               "Malaria prevalence according to microscopy"))
  # Note: This dataset does not include a WHO Region column

  # wmr2018f ####
  lup <- c("Lower", "Point", "Upper")
  wmr2018f <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-f.xls"),
                                 range="A4:I881",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Year",
                                   "Population at risk",
                                   paste0("Cases_",lup),
                                   paste0("Deaths_",lup)
                                 )) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA)

  # wmr2018g ####
  pc <- c("Presumed", "Confirmed")
  wmr2018g <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-g.xls"),
                                 range="A4:L100",
                                 col_names = c(
                                   "WHO region Country/area",
                                   "UN population",
                                   "At risk (low + high)",
                                   "At risk (high)",
                                   "Number of people living in active foci",
                                   paste0("Public sector ", pc),
                                   "BLANK1",
                                   paste0("Private sector ", pc),
                                   paste0("Community level ", pc)
                                 )) |>
    dplyr::select(!dplyr::starts_with("BLANK")) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
  # NOTE: Tanzania is split into Mainland and Zanzibar
  #       When making the summary table do not double count Tanzania
  # Also the summary table A104:K109 differs in Pub/Com Presumed cases for the Americas
  # One would expect 0 from the data but gets 20057 and	2 respectively in the regional summary
  # I confirmed all other values are the same using
  # =SUM(B5:B50)-B46 ; =SUM(B52:B70) ; =SUM(B72:B79) ; =SUM(B81:B89) ; =SUM(B91:B100)
  # Conditional format applies to =$L$105:$U$109 ; Cell value not equals `=B105`
  dplyr::mutate(dplyr::across(4:12, ~as.numeric(stringr::str_replace(.x,"-",""))))

  # wmr2018h ####
  wmr2018h <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-h.xls"),
                                 range="A2:J631",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Variable",
                                   2010:2017
                                 )) |>
    # Note that EUROPEAN region is less common in other datasets (less malaria)
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(across(4:11, ~as.numeric(stringr::str_replace(.x,"-",""))))
  # NOTE: Tanzania, Mainland and Zanzibar are split again
  # I did not check the Regional Summary

  wmr2018i <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-i.xls"),
                                 range="A2:J423",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Species",
                                   2010:2017
                                 )) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(dplyr::across(4:11, ~as.numeric(stringr::str_replace(.x,"-",""))))

  # wmr2018j ####
  wmr2018j <- readxl::read_excel(file.path(basePath,"wmr2018-annex-table-j.xls"),
                                 range="A2:I111",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   2010:2017
                                 )) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(dplyr::across(3:10, ~as.numeric(stringr::str_replace(.x,"-",""))))

  list(
    # Annex 3
    # A. Policy adoption, 2017
    wmr2018a = wmr2018a,
    # B. Antimalarial drug policy, 2017
    wmr2018b = wmr2018b,
    # C. Funding for malaria control, 2015–2017
    wmr2018c = wmr2018c,
    # D. Commodities distribution and coverage, 2015–2017
    wmr2018d = wmr2018d,
    # E. Household survey results, 2015–2017
    wmr2018e = wmr2018e,
    # F. Population at risk and estimated malaria cases and deaths, 2010–2017
    wmr2018f = wmr2018f,
    # G. Population at risk and reported malaria cases by place of care, 2017
    wmr2018g = wmr2018g,
    # H. Reported malaria cases by method of confirmation, 2010–2017
    wmr2018h = wmr2018h,
    # I. Reported malaria cases by species, 2010–2017
    wmr2018i = wmr2018i,
    # J. Reported malaria deaths, 2010–2017
    wmr2018j = wmr2018j
  )
}

wmr2018 <- get_wmr2018()

usethis::use_data(wmr2018, overwrite = TRUE)
