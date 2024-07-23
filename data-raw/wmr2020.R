# The `ensure_annex_dirs_exist` function fetches raw data from the WHO website
# and unzips it to a local directory.
# This file brings the unzipped data from the 2020 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2020 page 123:
# Annex 1 - Data sources and methods
# Annex 2 - Regional profiles
# > A. WHO African Region
# a. West Africa
# b. Central Africa
# c. Countries with high transmission in East and Southern Africa
# d. Countries with low transmission in East and Southern Africa
# > B. WHO Region of the Americas
# > C. WHO Eastern Mediterranean Region
# > D. WHO South-East Asia Region
# > E. WHO Western Pacific Region
# Annex 3 - Data tables
# > A. Policy adoption, 2019
# > B. Antimalarial drug policy, 2019
# > C. Funding for malaria control, 2017–2019
# > D. Commodities distribution and coverage, 2017–2019
# > Ea. Household survey results, 2015–2019, compiled through STATcompiler
# > Eb. Household survey results, 2015–2019, compiled through WHO calculations
# > F. Population denominator for case incidence and mortality rate, and
# estimated malaria cases and deaths, 2000–2019
# > G. Population denominator for case incidence and mortality rate, and reported
# malaria cases by place of care, 2019
# > H. Reported malaria cases by method of confirmation, 2010–2019
# > I. Reported malaria cases by species, 2010–2019
# > J. Reported malaria deaths, 2010–2019

get_wmr2020 <- function() {
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2020)
  # raw\unzipped\wmr-2020-excel-annexes\wmr-2020-excel-annexes\wmr-2020-excel-annexes(2021-03-11)
  basePath <- file.path(names(fileTree)[[1]], "wmr-2020-excel-annexes", "wmr-2020-excel-annexes(2021-03-11)")

  # wmr2020a ####
  policy_dict <- c(
    "Y1" = "Policy adopted and implemented this year. Available data from the world malaria report data collection form provides evidence for implementation.",
    "Y" = "Policy adopted but not implemented this year (2019) or no supportive available data reported to WHO.",
    "N" = "Policy not adopted.",
    "D" = "Policy  discontinued.",
    "B" = "Both microscopy and RDT are free.",
    "Y2" = "Diagnosis is free but diagnostic test was not specified.",
    "MIC" = "Only microscopy is free.",
    "RDT" = "Only RDT is free.",
    "Ni" = "Neither microscopy or RDT are free.",
    "NA" = "Question not applicable.",
    "–" = "Question not answered and there is no information from previous years."
  )
  wmr2020a <- readxl::read_excel(file.path(basePath, "Annex 3 - A. Policy adoption, 2019.xlsx"),
    sheet = "DataTable-3A",
    range = "A6:R102"
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(3:19, ~ case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area` != "United Republic of Tanzania3")

  ## WMR2020A Assertions: ####
  check_who_dataframe(
    df = wmr2020a,
    rows = 90,
    cols = 19,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 90
    ),
    na_values = list(
      c(29, "ITNs/LLINs are distributed free of charge")
    ),
    known_values = list(
      # The last possible value in the last column
      c(90, "Directly observed treatment with primaquine is undertaken", "Policy adopted and implemented this year. Available data from the world malaria report data collection form provides evidence for implementation.")
    )
  )

  # wmr2020b ####
  wmr2020b <- readxl::read_excel(file.path(basePath, "Annex 3 - B Antimalarial drug policy 2019 COPYEDIT.xlsx"),
    sheet = "DrugPolicy", # There are hidden sheets
    range = "A4:F105",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::rename(
      `Uncomplicated unconfirmed` = `Uncomplicated\r\nunconfirmed`,
      `Uncomplicated confirmed` = `Uncomplicated\r\nconfirmed`
    )

  ## WMR2020B Assertions: ####
  check_who_dataframe(
    df = wmr2020b,
    rows = 96,
    cols = 7,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 93
    ),
    na_values = list(
      c(2, "Uncomplicated unconfirmed")
    ),
    known_values = list(
      # The last possible value in the last column
      c(91, "Treatment", "CQ+PQ")
    )
  )


  # wmr2020c ####
  wmr2020c <- readxl::read_excel(file.path(basePath, "Annex 3 - C Funding for malaria control 2017-2019.xlsx"),
    sheet = "Funding", # There are hidden sheets
    range = "A6:O289",
    col_names = c(
      "WHO region\nCountry/Area",
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
      "Country_Other contributions"
    )
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(
      # Translate the footnotes into a boolean column
      `Country_Government Footnotes` = case_match_dict(`Country_Government Footnotes`, dict = c(
        "5" = "Budget not expenditure",
        "6" = "WHO NMP funding estimates"
      ))
    ) |>
    try_make_numeric(cols = 4:8)

  ## WMR2020C Assertions: ####
  check_who_dataframe(
    df = wmr2020c,
    rows = 279,
    cols = 16,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 92
    ),
    na_values = list(
      c(277, "Year")
    ),
    known_values = list(
      # The last possible value in the last column
      c(276, "Country_Other contributions", 385000)
    )
  )

  # wmr2020d ####
  wmr2020d <- readxl::read_excel(file.path(basePath, "Annex 3 - D Commodities distribution and coverage 2017-2019.xlsx"),
    sheet = "Table D (Coverage)",
    range = "A4:H297",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2020D Assertions: ####
  check_who_dataframe(
    df = wmr2020d,
    rows = 288,
    cols = 9,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 96
    ),
    na_values = list(
      c(3, "No. of LLINs sold or delivered")
    ),
    known_values = list(
      # The last possible value in the last column
      c(288, "No. of malaria cases treated with any first-line treatment courses (including ACT)", 5892)
    )
  )

  # wmr2020e ####
  wmr2020ea <- readxl::read_excel(file.path(basePath, "Annex 3 - Ea. HH survey results 2015-2019 STATcompiler-COPYEDIT.xlsx"),
    sheet = "annex_3e",
    range = "A6:T44",
    na = c("", "-"),
    col_names = c(
      "WHO Region/Country/area", "Source",
      paste("% of households with", c(
        "at least one ITN",
        "at least one ITN for every two persons who stayed in the household the previous night",
        "IRS in last 12 months",
        "at least one ITN and/or IRS in the past 12 months",
        "at least one ITN for every two persons and/or IRS in the past 12 months"
      )),
      paste("% of population", c(
        "with access to an ITN",
        "who slept under an ITN last night"
      )),
      "% of ITNs that were used last night",
      paste("% of pregnant women", c(
        "who slept under an ITN",
        "who took 3+ doses of IPTp"
      )),
      paste("% of children <5 years", c(
        "who slept under an ITN",
        "with moderate or severe anaemia",
        "with a positive RDT",
        "with a positive microscopy blood smear"
      )),
      paste("% of children <5 years with fever in last 2 weeks", c(
        "for whom advice or treatment was sought",
        "who had blood taken from a finger or heel for testing",
        "who took antimalarial drugs",
        "who took an ACT among those who received any antimalarial"
      ))
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2020EA Assertions: ####
  check_who_dataframe(
    df = wmr2020ea,
    rows = 37,
    cols = 21,
    unique_values = list(
      `WHO Region` = 2,
      `Country/area` = 26
    ),
    na_values = list(
      c(6, "% of households with at least one ITN")
    ),
    known_values = list(
      # The last possible value in the last column
      c(34, "% of children <5 years with fever in last 2 weeks who took an ACT among those who received any antimalarial", 96.9)
    )
  )

  # wmr2020eb ####
  wmr2020eb <- readxl::read_excel(file.path(basePath, "Annex 3 - Eb HH survey results, 2015-2019 WHO calculations COPYEDIT.xlsx"),
    sheet = "Appendix",
    range = "A5:Z29",
    na = c("", "-", "–"),
    # Major names:
    # "Fever prevalence", "Health sector where treatment was sought", "Diagnostic testing coverage in each health sector", "Antimalarial treatment coverage in each health sector", "ACT use among antimalarial treatment in each health sector"
    col_names = c(
      "WHO Region/Country/area",
      "Survey",
      "Fever prevalence Overall",
      paste("Health sector where treatment was sought", c(
        "Public excluding community health workers",
        "Community health workers",
        "Formal medical private excluding pharmacies",
        "Pharmacies or accredited drug stores",
        "Informal private",
        "No treatment seeking",
        "Trained provider"
      )),
      paste("Diagnostic testing coverage in each health sector", c(
        "Public excluding community health workers",
        "Community health workers",
        "Formal medical private excluding pharmacies",
        "Pharmacies or accredited drug stores",
        "Informal private",
        "Trained provider"
      )),
      paste("Antimalarial treatment coverage in each health sector", c(
        "Public excluding community health workers",
        "Community health workers",
        "Formal medical private excluding pharmacies",
        "Pharmacies or accredited drug stores",
        "Self-treatment",
        "No treatment seeking",
        "Trained provider"
      )),
      paste("ACT use among antimalarial treatment in each health sector", c(
        "Public",
        "Private",
        "Informal private"
      ))
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2020EB Assertions: ####
  check_who_dataframe(
    df = wmr2020eb,
    rows = 24,
    cols = 27,
    unique_values = list(
      `WHO Region` = 1,
      `Country/area` = 24
    ),
    na_values = list(
      c(6, "Diagnostic testing coverage in each health sector Public excluding community health workers")
    ),
    known_values = list(
      # The last possible value in the last column
      c(15, "ACT use among antimalarial treatment in each health sector Informal private", "35\r\n(22, 50)")
    )
  )

  # wmr2020f ####
  lup <- c("Lower", "Point", "Upper")
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2020f <- readxl::read_excel(file.path(basePath, "Annex 3 - F PAR and estimated malaria cases and deaths 2000-2019.xlsx"),
    sheet = "Appendix F",
    range = "A7:J2172",
    col_names = c(
      "BLANK",
      "WHO region/Country/area",
      "Year",
      "Population denominator for incidence and mortality rate",
      paste0("Cases_", lup),
      paste0("Deaths_", lup)
    )
  ) |>
    dplyr::select(!BLANK) |>
    dplyr::mutate(`WHO region/Country/area` = ifelse(`WHO region/Country/area` %in% who_regions,
      toupper(`WHO region/Country/area`),
      `WHO region/Country/area`
    )) |>
    split_who_region(col_region_area = 1)

  ## WMR2020F Assertions: ####
  check_who_dataframe(
    df = wmr2020f,
    rows = 2160,
    cols = 10,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 108,
      `Year` = 20
    ),
    known_values = list(
      # The last possible value in the last column
      c(2160, "Deaths_Upper", "29")
    )
  )

  # wmr2020g ####
  pcf <- c("Presumed", "Confirmed", "Footnotes")
  footnote_dict <- c(
    "3"   = "Figures reported for the public sector include cases detected at the community level.",
    "4"   = "Figures reported for the public sector include cases detected in the private sector.",
    "5"   = "Figures reported for the public sector include cases detected at the community level and in the private sector."
  )
  {
    # On my Windows machine the path was too long so I had to copy the file to a shorter path to read it:
    fname <- file.path(basePath, "Annex 3 - G. Population denominator for case incidence and mortality rate, and reported malaria cases by place of care, 2019.xlsx")
    smallerPath <- file.path(tempdir(), "wmr2020g.xlsx")
    file.copy(fname, smallerPath, overwrite = TRUE)
    wmr2020g <- readxl::read_excel(smallerPath,
      sheet = "Table G (Cases)",
      range = "A8:M104",
      na = c("", "-"),
      col_names = c(
        "WHO region Country/area",
        "UN population",
        "At risk (low + high)",
        "At risk (high)",
        "Number of people living in active foci",
        paste("Public sector", pcf),
        paste("Private sector", pcf),
        "Community level Presumed",
        "Community level Confirmed"
      )
    ) |>
      dplyr::select(!dplyr::starts_with("BLANK")) |>
      split_who_region(col_region_area = 1) |>
      dplyr::mutate(dplyr::across(tidyselect::contains("Footnote"), ~ case_match_dict(as.character(.x), footnote_dict)))
    file.remove(smallerPath)
  }
  ## WMR2020G Assertions: ####
  check_who_dataframe(
    df = wmr2020g,
    rows = 92,
    cols = 14,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 92
    ),
    na_values = list(
      c(29, "UN population")
    ),
    known_values = list(
      # The last possible value in the last column
      c(88, "Community level Confirmed", 3311)
    )
  )

  # wmr2020h ####
  wmr2020h <- readxl::read_excel(file.path(basePath, "Annex 3 - H. Reported malaria cases by method of confirmation, 2010–2019.xlsx"),
    sheet = "AnnexH",
    range = "A6:L693",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Variable",
      2010:2019
    )
  ) |>
    # Note that EUROPEAN region is less common in other datasets (less malaria)
    split_who_region(col_region_area = 1)
  # NOTE: Tanzania, Mainland and Zanzibar are split again with values in each
  # I did not check the Regional Summary

  ## WMR2020H Assertions: ####
  check_who_dataframe(
    df = wmr2020h,
    rows = 682,
    cols = 13,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(5, "2010")
    ),
    known_values = list(
      # Namibia in 2011 has a footnote
      c(195, "2011", "14071**"),
      # The last possible value in the last column
      c(682, "2019", "1565")
    )
  )

  # wmr2020i ####
  wmr2020i <- readxl::read_excel(file.path(basePath, "Annex 3 - I. Reported malaria cases by species, 2010–2019.xlsx"),
    sheet = "Data-Table-3I",
    range = "A5:L634",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Species",
      2010:2019
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2020I Assertions: ####
  check_who_dataframe(
    df = wmr2020i,
    rows = 624,
    cols = 13,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(2, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(624, "2019", "1565")
    )
  )

  # wmr2020j ####
  wmr2020j <- readxl::read_excel(file.path(basePath, "Annex 3 - J Reported malaria deaths 2010-2019 COPYEDIT.xlsx"),
    sheet = "RepDeaths",
    range = "A4:K113",
    na = c("", "-", "–"),
    col_names = c(
      "WHO region/Country/area",
      2010:2019
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2020J Assertions: ####
  check_who_dataframe(
    df = wmr2020j,
    rows = 104,
    cols = 12,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(12, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(104, "2019", 0)
    )
  )

  list(
    # Annex 3
    # > A. Policy adoption, 2019
    wmr2020a = wmr2020a,
    # > B. Antimalarial drug policy, 2019
    wmr2020b = wmr2020b,
    # > C. Funding for malaria control, 2017–2019
    wmr2020c = wmr2020c,
    # > D. Commodities distribution and coverage, 2017–2019
    wmr2020d = wmr2020d,
    # > Ea. Household survey results, 2015–2019, compiled through STATcompiler
    wmr2020ea = wmr2020ea,
    # > Eb. Household survey results, 2015–2019, compiled through WHO calculations
    wmr2020eb = wmr2020eb,
    # > F. Population denominator for case incidence and mortality rate, and
    # estimated malaria cases and deaths, 2000–2019
    wmr2020f = wmr2020f,
    # > G. Population denominator for case incidence and mortality rate, and reported
    # malaria cases by place of care, 2019
    wmr2020g = wmr2020g,
    # > H. Reported malaria cases by method of confirmation, 2010–2019
    wmr2020h = wmr2020h,
    # > I. Reported malaria cases by species, 2010–2019
    wmr2020i = wmr2020i,
    # > J. Reported malaria deaths, 2010–2019
    wmr2020j = wmr2020j
  )
}

wmr2020 <- get_wmr2020()

usethis::use_data(wmr2020, overwrite = TRUE)
