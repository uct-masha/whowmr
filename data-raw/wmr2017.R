# The downloadData.R file exposes `ensureRawDataExists` which fetches raw data
# from the WHO website and unzips it to a local directory.
# This file brings the unzipped data from the 2017 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2017 page 65:
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
# >> A. Policy adoption, 2016
# >> B. Antimalarial drug policy, 2016
# >> C. Funding for malaria control, 2014–2016
# >> D. Commodities distribution and coverage, 2014–2016
# >> E. Household survey results, 2014–2016
# >> F.a. Estimated malaria cases and deaths, 2010–2016
# >> F.b. Population at risk for estimates of malaria cases and deaths, 2010–2016
# >> G. Population at risk and reported malaria cases by place of care, 2016
# >> H. Reported malaria cases by method of confirmation, 2010–2016
# >> I. Reported malaria cases by species, 2010–2016
# >> J. Reported malaria deaths, 2010–2016

get_wmr2017 <- function() {
  # First we need to ensure the unzipped annex file exists:
  source('data-raw/downloadData.R')
  fileTree <- ensureRawDataExists(2017)
  basePath <- names(fileTree)[[1]]
  # TODO: Remove footnotes in column names
  # TODO: Remove footnotes in cell values

  # wmr2017a ####
  to_T_F_NA <- Vectorize(function(x) {
    if(is.na(x)) return(NA)
    if(x=="Y") return(T)
    if(x=="N") return(F)
    NA
  })
  wmr2017a <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-a.xls"), range = "A2:Q102") |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(dplyr::across(3:18, to_T_F_NA)) |>
    dplyr::filter(`Country/area`!="United Republic of Tanzania3")

  # wmr2017b ####
  wmr2017b <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-b.xls"), range = "A2:F102") |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::rename(`Uncomplicated unconfirmed`=`Uncomplicated\nunconfirmed`,
                  `Uncomplicated confirmed`=`Uncomplicated\nconfirmed`)

  # wmr2017c ####
  wmr2017c <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-c.xls"),
                                 range = "A3:O286",
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

  # wmr2017d ####
  wmr2017d <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-d.xls"), range = "A1:I294") |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    # column 7 is irs coverage and includes <1 so we impose no parsing to numeric there
    dplyr::mutate(dplyr::across(setdiff(4:10,7), ~as.numeric(stringr::str_replace_all(.x,pattern = "[-']",replacement = ""))))

  # wmr2017e ####
  wmr2017e <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-e.xlsx"),
                                 sheet="DATA", range="A6:Q106",
                                 col_names = c("Country", "Source",
                                               "Households with at least one insecticide-treated mosquito net (ITN)",
                                               "Households with at least one insecticide-treated mosquito net (ITN) for every two persons who stayed in the household the previous night",
                                               "Households with indoor residual spraying (IRS) in last 12 months",
                                               "Households with at least one insecticide-treated mosquito net (ITN) for every two persons and/or indoor residual spraying (IRS) in the past 12 months",
                                               "Persons with access to an insecticide-treated mosquito net (ITN)",
                                               "Population who slept under an insecticide-treated mosquito net (ITN) last night",
                                               "Existing insecticide-treated mosquito nets (ITNs) used last night",
                                               "Children under 5 who slept under an insecticide-treated net (ITN)",
                                               "Pregnant women who slept under an insecticide-treated net (ITN)",
                                               "SP/Fansidar 3+ doses, at least one during ANC visit (IPTp)",
                                               "Children with fever for whom advice or treatment was sought",
                                               "Children with fever who had blood taken from a finger or heel for testing",
                                               "Children who took any ACT", "Children with hemoglobin lower than 8.0 g/dl",
                                               "Malaria prevalence according to microscopy"))
  # Note: This dataset does not include a WHO Region column

  # wmr2017f ####
  lup <- c("Lower", "Point", "Upper")
  wmr2017f <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-f.xlsx"),
                                 range="A3:J695", sheet="Burden",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Year",
                                   "Blank1",
                                   paste0("Cases_",lup),
                                   "Blank2",
                                   paste0("Deaths_",lup)
                                 )) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::select(!dplyr::starts_with("Blank")) |>
    # Some cells have, eg, <=100 cases and we don't want to impose parsing there
    # so the user can interpret that as they wish. To maintain consistent
    # formatting then, we just ensure Cases/Deaths columns are all characters.
    dplyr::mutate(dplyr::across(4:9, as.character))

  # wmr2017fa ####
  wmr2017fa <- wmr2017f  # confirmed with all.equal

  # wmr2017fb ####
  # Note: fb has 2 sheets. The first is a pivot of the second. We will read the
  # second since it is more detailed.
  wmr2017fb <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-f-b.xlsx"),
                                  range="A2:D1752", sheet=2,
                                  col_names <- c(
                                    "WHO Region",
                                    "Country/area",
                                    "Year",
                                    "Population at Risk"
                                  )) |>
    # Following the convention that WHO REGION is uppercase
    dplyr::mutate(`WHO Region` = toupper(`WHO Region`))

  # wmr2017g ####
  pc <- c("Presumed", "Confirmed")
  wmr2017g <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-g.xls"),
                                 range="A4:K100",
                                 col_names = c(
                                   "WHO region Country/area",
                                   "UN population",
                                   "At risk (low + high)",
                                   "At risk (high)",
                                   "Number of people living in active foci",
                                   paste0("Public sector ", pc),
                                   paste0("Private sector ", pc),
                                   paste0("Community level ", pc)
                                 )) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
  # NOTE: Tanzania is split into Mainland and Zanzibar
  #       When making the summary table do not double count Tanzania
  # Also the summary table A104:K109 differs in Pub/Com Presumed cases for the Americas
  # One would expect 0 from the data but gets 20057 and	2 respectively in the regional summary
  # I confirmed all other values are the same using
  # =SUM(B5:B50)-B46 ; =SUM(B52:B70) ; =SUM(B72:B79) ; =SUM(B81:B89) ; =SUM(B91:B100)
  # Conditional format applies to =$L$105:$U$109 ; Cell value not equals `=B105`
    dplyr::mutate(dplyr::across(4:12, ~as.numeric(stringr::str_replace(.x,"-",""))))

  # wmr2017h ####
  wmr2017h <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-h.xls"),
                                 range="A2:I631",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Variable",
                                   2010:2016
                                 )) |>
    # In this file the WHO Region was not entered correctly. We manually add it
    # here:
    dplyr::mutate(`WHO region/Country/area`=dplyr::case_match(seq_along(`WHO region/Country/area`)+1,
                                                         2 ~ "AFRICAN", # Algeria:Zimbabwe
                                                       285 ~ "AMERICAS", # Argentina:Venezuela
                                                       412 ~ "EASTERN MEDITERRANEAN", # Afghanistan:Yemen
                                                       461 ~ "EUROPEAN", # Armenia:Uzbekistan
                                                       510 ~ "SOUTH-EAST ASIA", # Bangladesh:Timor-Leste
                                                       571 ~ "WESTERN PACIFIC", # Cambodia:Viet Nam
                                                       .default = `WHO region/Country/area`
    )) |>
    # Note that EUROPEAN region is uncommon in other datasets
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(across(4:10, ~as.numeric(stringr::str_replace(.x,"-",""))))
  # NOTE: Tanzania, Mainland and Zanzibar are split again
  # I did not check the Regional Summary

  # wmr2017i ####
  wmr2017i <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-i.xls"),
                                 range="A2:I423",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Species",
                                   2010:2016
                                 )) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(dplyr::across(4:10, ~as.numeric(stringr::str_replace(.x,"-",""))))

  # wmr2017j ####
  wmr2017j <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-j.xls"),
                                 range="A2:H111",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   2010:2016
                                 )) |>
    whowmr::split_who_region(col_region_area=1, col_na_when_region=NA) |>
    dplyr::mutate(dplyr::across(3:9, ~as.numeric(stringr::str_replace(.x,"-",""))))

  # combined ####
  list(
    # Annex 3
    # A: Policy Adoption, 2016
    wmr2017a = wmr2017a,
    # B: Antimalarial drug policy, 2016
    wmr2017b = wmr2017b,
    # C: Funding for malaria control, 2014–2016
    wmr2017c = wmr2017c,
    # D: Commodities distribution and coverage, 2014–2016
    wmr2017d = wmr2017d,
    # E: Household survey results, 2014–2016
    wmr2017e = wmr2017e,
    # wmr2017f = wmr2017f,
    # F.a: Estimated malaria cases and deaths, 2010–2016
    wmr2017fa = wmr2017fa,
    # F.b: Population at risk for estimates of malaria cases and deaths, 2010–2016
    wmr2017fb = wmr2017fb,
    # G: Population at risk and reported malaria cases by place of care, 2016
    wmr2017g = wmr2017g,
    # H: Reported malaria cases by method of confirmation, 2010–2016
    wmr2017h = wmr2017h,
    # I: Reported malaria cases by species, 2010–2016
    wmr2017i = wmr2017i,
    # J: Reported malaria deaths, 2010–2016
    wmr2017j = wmr2017j
  )
}

wmr2017 <- get_wmr2017()

usethis::use_data(wmr2017, overwrite = TRUE)
