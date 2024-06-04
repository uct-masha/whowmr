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
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr", "stringr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2017)
  basePath <- names(fileTree)[[1]]
  # TODO: Remove footnotes in column names?
  # TODO: Remove footnotes in cell values?

  # wmr2017a ####
  policy_dict <- c(
    "Y" = "Actually implemented",
    "N" = "Not implemented",
    "-" = "Question not answered or not applicable",
    "NA" = "Not applicable",
    `NA`=NA
  )
  wmr2017a <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-a.xls"),
                                 sheet="National Policy",
                                 range = "A2:Q102") |>
    split_who_region(col_region_area=1) |>
    dplyr::mutate(dplyr::across(3:18, ~case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area`!="United Republic of Tanzania3")

  ## WMR2017A Assertions: ####
  check_who_dataframe(df = wmr2017a,
                      rows = 94,
                      cols = 18,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 94
                      ),
                      known_values = list(
                        # cell D50 is Zimbabwe and does not distribute ITNs through mass campaigns
                        list(46, "ITNs/ LLINs distributed through mass campaigns to all age groups", val="Not implemented"),
                        # cell A102 is "Viet Nam" and is in the Western Pacific region
                        list(94, "WHO Region", val="WESTERN PACIFIC"),
                        list(94, "Country/area", val="Viet Nam")
                      )
  )

  # wmr2017b ####
  wmr2017b <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-b.xls"),
                                 sheet="DrugPolicy",
                                 range = "A2:F102") |>
    split_who_region(col_region_area=1) |>
    dplyr::rename(`Uncomplicated unconfirmed`=`Uncomplicated\nunconfirmed`,
                  `Uncomplicated confirmed`=`Uncomplicated\nconfirmed`)

  ## WMR2017B Assertions: ####
  check_who_dataframe(df = wmr2017b,
                      rows = 95,
                      cols = 7,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 95
                      ),
                      # cell B4 is empty for Algeria which is na
                      na_values = list(
                        c(1,"Uncomplicated unconfirmed")
                      ),
                      known_values = list(
                        # cell F52 is Argentina in the Americas with CQ+PQ for P.vivax Treatment
                        list(48, "WHO Region", val="AMERICAS"),
                        list(48, "Country/area", val="Argentina"),
                        list(48, "Treatment", val="CQ+PQ")
                      )
  )

  # wmr2017c ####
  wmr2017c <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-c.xls"),
                                 sheet="Funding",
                                 range = "A3:O286",
                                 col_names = c("WHO region\nCountry/Area",
                                               "Year",
                                               "Donor_Global Fund",
                                               "Donor_PMI/USAID",
                                               "Donor_The World Bank",
                                               "Donor_UK",
                                               "Country_Government (NMP)",
                                               # column 8 holds footnotes on how to interpret column 7
                                               "Country_Government Footnotes",
                                               "Country_Global Fund",
                                               "Country_The World Bank",
                                               "Country_PMI/USAID",
                                               "Country_Other bilaterals",
                                               "Country_WHO",
                                               "Country_UNICEF",
                                               "Country_Other contributions")) |>
    split_who_region(col_region_area=1) |>
    dplyr::mutate(
      # Translate the footnotes into a boolean column
      `Country_Government Footnotes`=case_match_dict(`Country_Government Footnotes`, dict=c(
                                        "5"="Budget not expenditure"))) |>
    try_make_numeric(cols=setdiff(4:16,9),
                     str_remove="[’']+")
          # Note nothing for Algeria or Argentina
          # Some cells (eg Pakistan 2016) reported eg "16’400’000"
          # which won't parse well, so we clean that up before parsing:
  ## WMR2017C Assertions: ####
  check_who_dataframe(df = wmr2017c,
                      rows = 279,
                      cols = 16,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 93
                      ),
                      # cell D4 is "-" which is na
                      na_values = list(
                        list(1, "Country_Government Footnotes"), # H4, Algeria, 2014
                        list(136, "Country_WHO") # M139, Zimbabwe, 2016
                      ),
                      known_values = list(
                        # cell G215 is Pakistan 2016 with value "16’400’000"
                        list(210, "WHO Region", "EASTERN MEDITERRANEAN"),
                        list(210, "Country/area", "Pakistan"),
                        list(210, "Country_Government (NMP)", "16400000")
                      )
  )

  # wmr2017d ####
  wmr2017d <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-d.xls"),
                                 sheet = "Presentation",
                                 na="-",
                                 range = "A1:I294") |>
    split_who_region(col_region_area=1) |>
    # column 7 is irs coverage and includes <1 so we impose no parsing to numeric there
    try_make_numeric(setdiff(4:10,7),
                     str_remove="[-']+")

  ## WMR2017D Assertions: ####
  check_who_dataframe(df = wmr2017d,
                      rows = 288,
                      cols = 10,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 96
                      ),
                      # cell D4 is "-" which is na
                      na_values = list(
                        list(1, "IRS coverage (%)") # H4, Algeria, 2014
                      ),
                      known_values = list(
                        # C226 (Somalia 2016) is 655'798
                        list(222, "WHO Region", "EASTERN MEDITERRANEAN"),
                        list(222, "Country/area", "Somalia"),
                        list(222, "No. of LLIN sold or delivered", 655798),
                        # # F294 (last row) is <1 IRS cov and 417142 IRS protected
                        list(288, "WHO Region", "WESTERN PACIFIC"),
                        list(288, "Country/area", "Viet Nam"),
                        list(288, 6, 417142),
                        list(288, "IRS coverage (%)", "<1")
                      )
  )

  # wmr2017e ####
  wmr2017e <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-e.xlsx"),
                                 sheet="Annex 4-E", range="A3:Q35",
                                 c("WHO Region/Country/area", "Source",
                                   "% of HH that have at least one ITN",
                                   "% of HH with at least one ITN for every two persons who slept in the household the previous night",
                                   "% of the population with access to an ITN in their household",
                                   "% of existing ITNs in HH used the previous night",
                                   "% of the population who slept under an ITN the previous night",
                                   "% of children <5 years who slept under an ITN the previous night",
                                   "% of pregnant women who slept under an ITN the previous night",
                                   "% of HH sprayed by IRS within last 12 months",
                                   "% of HH with = 1 ITN for 2 pers. and/or sprayed by IRS within last 12 months",
                                   "% of women who received at least 3 doses of IPT during ANC visits during their last pregnancy",
                                   "% of children aged 6-59 months with a hemoglobin measurement <8 g/dL",
                                   "% of children aged 6-59 months with a positive microscopy blood smear",
                                   "% of children <5 years with fever in last 2 weeks for whom advice or treatment was sought",
                                   "% of children <5 years with fever in last 2 weeks who received an ACT among those who received any antimalarial",
                                   "% of children <5 years with fever in last 2 weeks who had a finger or heel stick"
                                 )) |>
    split_who_region(col_region_area=1)

  ## WMR2017E Assertions: ####
  check_who_dataframe(df = wmr2017e,
                      rows = 28,
                      cols = 18,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 23
                      ),
                      # cell D4 is "-" which is na
                      na_values = list(
                        list(5, 13), # L8 Ethiopia
                        list(23, "% of women who received at least 3 doses of IPT during ANC visits during their last pregnancy") # L26, Zimbabwe
                      ),
                      known_values = list(
                        # second last row is Myanmar with last value in Q33 = 3
                        list(27, "WHO Region", "SOUTH-EAST ASIA REGION"),
                        list(27, "Country/area", "Myanmar"),
                        list(27, "% of children <5 years with fever in last 2 weeks who had a finger or heel stick", 3)
                      )
  )

  # wmr2017f ####
  lup <- c("Lower", "Point", "Upper")
  wmr2017f <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-f.xlsx"),
                                 sheet="Burden",
                                 range="A3:J695",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Year",
                                   "Blank1",
                                   paste0("Cases_", lup),
                                   "Blank2",
                                   paste0("Deaths_", lup)
                                 )) |>
    split_who_region(col_region_area=1) |>
    dplyr::select(!dplyr::starts_with("Blank")) |>
    # Some cells have, eg, <=100 cases and we don't want to impose parsing there
    # so the user can interpret that as they wish. To maintain consistent
    # formatting then, we just ensure Cases/Deaths columns are all characters.
    dplyr::mutate(dplyr::across(4:9, as.character))

  ## WMR2017F Assertions: ####
  check_who_dataframe(df = wmr2017f,
                      rows = 686,
                      cols = 9,
                      unique_values = list(
                        `WHO Region` = 7,
                        `Country/area` = 98
                      ),
                      # cell
                      na_values = list(
                        list(445, "Deaths_Lower") # H450 is Afghanistan 2013, NA, and surrounded by non-NA
                      ),
                      known_values = list(
                        # cell D573 is Timor-Leste 2015 with <100 Cases_Lower and 0 Deaths_Point
                        list(566, "WHO Region", "SOUTH-EAST ASIA"),
                        list(566, "Country/area", "Timor-Leste"),
                        list(566, "Deaths_Point", "0"),
                        list(566, "Cases_Lower", "≤100")
                      )
  )

  # wmr2017fa ####
  wmr2017fa <- wmr2017f  # confirmed with all.equal

  # wmr2017fb ####
  # Note: fb has 2 sheets. The first is a pivot of the second. We will read the
  # second since it is more detailed.
  wmr2017fb <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-f-b.xlsx"),
                                  sheet="NE PAS UTILISER", # The pivot table used for the table uses this sheet
                                  range="A2:D1752",
                                  col_names <- c(
                                    "WHO Region",
                                    "Country/area",
                                    "Year",
                                    "Population at Risk"
                                  )) |>
    # Following the convention that WHO REGION is uppercase
    dplyr::mutate(`WHO Region` = toupper(`WHO Region`))

  ## WMR2017FB Assertions: ####
  check_who_dataframe(df = wmr2017fb,
                      rows = 1751,
                      cols = 4,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 103
                      ),
                      known_values = list(
                        # cell D1379 is Turkmenistan with 0 Population at Risk
                        list(1378, "Population at Risk", 0),
                        # cell C1752 is Viet Nam 2016
                        list(1751, "Year", 2016)
                      )
  )

  # wmr2017g ####
  pc <- c("Presumed", "Confirmed")
  wmr2017g <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-g.xls"),
                                 sheet="CasesandDeaths",
                                 na="-",
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
    split_who_region(col_region_area=1)
  # NOTE: Tanzania is split into Mainland and Zanzibar
  #       When making the summary table do not double count Tanzania
  # Also the summary table A104:K109 differs in Pub/Com Presumed cases for the Americas
  # One would expect 0 from the data but gets 20057 and	2 respectively in the regional summary
  # I confirmed all other values are the same using
  # =SUM(B5:B50)-B46 ; =SUM(B52:B70) ; =SUM(B72:B79) ; =SUM(B81:B89) ; =SUM(B91:B100)
  # Conditional format applies to =$L$105:$U$109 ; Cell value not equals `=B105`

  ## WMR2017G Assertions: ####
  check_who_dataframe(df = wmr2017g,
                      rows = 92,
                      cols = 12,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 92
                      ),
                      # cell D10 is "-" which is na
                      na_values = list(
                        list(6, "At risk (high)")
                      ),
                      known_values = list(
                        # cell G52 is Belize with UN population 366954
                        list(47, "WHO Region", "AMERICAS"),
                        list(47, "Country/area", "Belize"),
                        list(47, "UN population", 366954)
                      )
  )

  # wmr2017h ####
  wmr2017h <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-h.xls"),
                                 sheet="Trend1",
                                 na="-",
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
    split_who_region(col_region_area=1)
  # NOTE: Tanzania, Mainland and Zanzibar are split again
  # I did not check the Regional Summary

  ## WMR2017H Assertions: ####
  check_who_dataframe(df = wmr2017h,
                      rows = 624,
                      cols = 10,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 104
                      ),
                      # cell I6 is "-" which is na
                      na_values = list(
                        list(4, "2016")
                      ),
                      known_values = list(
                        # Note: Cabo Verde 2016 is defined in 2017 data (I42) but not in 2018 data (S42)
                        list(40, "2016", 8906),
                        list(622, "WHO Region", "WESTERN PACIFIC"),
                        list(622, "Country/area", "Viet Nam"),
                        list(622, "2016", 408055)  # I629
                      )
  )

  # wmr2017i ####
  wmr2017i <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-i.xls"),
                                 sheet="Trend2",
                                 na="-",
                                 range="A2:I423",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Species",
                                   2010:2016
                                 )) |>
    split_who_region(col_region_area=1)

  ## WMR2017I Assertions: ####
  check_who_dataframe(df = wmr2017i,
                      rows = 416,
                      cols = 10,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 104
                      ),
                      # cell I387 is "-" which is na
                      na_values = list(
                        list(380, 10)
                      ),
                      known_values = list(
                        # I423 is Viet Nam with 15 "No Other" cases in 2016
                        list(416, "WHO Region", "WESTERN PACIFIC"),
                        list(416, "Country/area", "Viet Nam"),
                        list(416, "2016", 15)
                      )
  )

  # wmr2017j ####
  wmr2017j <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-j.xls"),
                                 sheet="RepDeaths",
                                 na="-",
                                 range="A2:H111",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   2010:2016
                                 )) |>
    split_who_region(col_region_area=1)

  ## WMR2017J Assertions: ####
  check_who_dataframe(df = wmr2017j,
                      rows = 104,
                      cols = 9,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 104
                      ),
                      # cell
                      na_values = list(
                        list(70, "2016") # H74, Djibouti
                      ),
                      known_values = list(
                        list(70, "WHO Region", "EASTERN MEDITERRANEAN"),
                        list(70, "Country/area", "Djibouti"),
                        list(104, "2016", 2) # H111, Viet Nam 2016
                      )
  )

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
