# The `ensure_annex_dirs_exist` function fetches raw data from the WHO website
# and unzips it to a local directory.
# This file brings the unzipped data from the 2022 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2022 page 131:
# Annex 1 - Data sources and methods
# Annex 2 - Number of ITNs distributed through campaigns in malaria
# endemic countries, 2020–2022
# Annex 3 - Regional profiles
# > A. WHO African Region
# a. West Africa
# b. Central Africa
# c. Countries with high transmission in east and southern Africa
# d. Countries with low transmission in east and southern Africa
# > B. WHO Region of the Americas
# > C. WHO Eastern Mediterranean Region
# > D. WHO South-East Asia Region
# > E. WHO Western Pacific Region
# Annex 4 - Data tables and methods
# > A. Policy adoption, 2021
# > B. Antimalarial drug policy, 2021
# > C. Funding for malaria control, 2019–2021
# > D. Commodities distribution and coverage, 2019–2021
# > Ea. Household survey results, 2017–2021, compiled through STATcompiler
# > Eb. Household survey results, 2017–2021, compiled through WHO calculations
# > F. Population denominator for case incidence and mortality rate, and
# estimated malaria cases and deaths, 2000–2021
# > G. Population denominator for case incidence and mortality rate, and reported
# malaria cases by place of care, 2021
# > H. Reported malaria cases by method of confirmation, 2010–2021
# > I. Reported malaria cases by species, 2010–2021
# > J. Reported malaria deaths, 2010–2021
# > K. Methods for Tables A-D-G-H-I-J

get_wmr2022 <- function() {
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2022)
  basePath <- file.path(names(fileTree)[[1]], "WMR2022_Annex")

  # wmr2022_2 ####
  wmr2022_2 <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_2.xlsx"),
    sheet = "Annex 2",
    range = "A4:J56",
    na = "NA"
  )

  check_who_dataframe(
    df = wmr2022_2,
    rows = 52,
    cols = 10,
    unique_values = list(
      `Country` = 52
    ),
    na_values = list(
      c(18, "Percentage of planned ITNs distributed in 2020")
    ),
    known_values = list(
      # The last possible value in the last column
      c(52, "Percentage of ITNs planned for distribution in 2021 distributed in 2021", 100)
    )
  )

  # wmr2022a ####
  policy_dict <- c(
    "Y1" = "Policy exists and has been implemented this year.",
    "Y" = "Policy exists but is not implemented this year or no data exists to support implementation.",
    "N" = "Policy does not exist.",
    "D" = "Policy discontinued.",
    "B" = "Both microscopy and RDT are free.",
    "Y2" = "Diagnosis is free but diagnostic test was not specified.",
    "MIC" = "Only microscopy is free.",
    "RDT" = "Only RDT is free.",
    "Ni" = "Neither microscopy nor RDT are free.",
    "NA" = "Policy not applicable.",
    "-" = "Question not answered and there is no information from previous years."
  )
  wmr2022a <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4A.xlsx"),
    sheet = "data",
    range = "A4:R99"
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(3:19, ~ case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area` != "United Republic of Tanzania2")

  ## WMR2022A Assertions: ####
  check_who_dataframe(
    df = wmr2022a,
    rows = 89,
    cols = 19,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 89
    ),
    known_values = list(
      # The last possible value in the last column
      c(89, "Directly observed treatment with primaquine is undertaken", "Policy exists but is not implemented this year or no data exists to support implementation.")
    )
  )

  # wmr2022b ####
  wmr2022b <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4B.xlsx"),
    sheet = "data",
    range = "A4:F99",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::rename(
      `Uncomplicated unconfirmed` = `Uncomplicated\r\nunconfirmed`,
      `Uncomplicated confirmed` = `Uncomplicated\r\nconfirmed`
    )

  ## WMR2022B Assertions: ####
  check_who_dataframe(
    df = wmr2022b,
    rows = 90,
    cols = 7,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 90
    ),
    na_values = list(
      c(2, "Uncomplicated unconfirmed")
    ),
    known_values = list(
      # The last possible value in the last column
      c(90, "Treatment", "CQ+PQ")
    )
  )

  # wmr2022c ####
  wmr2022c <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4C.xlsx"),
    sheet = "data",
    range = "A6:O277",
    na = c("", "–"),
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

  ## WMR2022C Assertions: ####
  check_who_dataframe(
    df = wmr2022c,
    rows = 267,
    cols = 16,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 89
    ),
    na_values = list(
      c(16, "Donor_Global Fund")
    ),
    known_values = list(
      # The last possible value in the last column
      c(267, "Country_Other contributions", 755652)
    )
  )

  # wmr2022d ####
  wmr2022d <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4D.xlsx"),
    sheet = "data",
    range = "A4:J279",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2022D Assertions: ####
  check_who_dataframe(
    df = wmr2022d,
    rows = 270,
    cols = 11,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 90
    ),
    na_values = list(
      c(9, "No. of LLINs delivered")
    ),
    known_values = list(
      # The last possible value in the last column
      c(269, "No. of malaria cases treated with ACT", 818)
    )
  )

  # wmr2022e ####
  wmr2022ea <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4Ea.xlsx"),
    sheet = "data",
    range = "A6:T40",
    na = "–",
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

  ## WMR2022EA Assertions: ####
  check_who_dataframe(
    df = wmr2022ea,
    rows = 31,
    cols = 21,
    unique_values = list(
      `WHO Region` = 4,
      `Country/area` = 26
    ),
    known_values = list(
      # The last possible value in the last column
      c(31, "% of children <5 years with fever in last 2 weeks who took an ACT among those who received any antimalarial", "-")
    )
  )

  # wmr2022eb ####
  wmr2022eb <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4Eb.xlsx"),
    sheet = "data",
    range = "A6:Z27",
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

  ## WMR2022EB Assertions: ####
  check_who_dataframe(
    df = wmr2022eb,
    rows = 21,
    cols = 27,
    unique_values = list(
      `WHO Region` = 1,
      `Country/area` = 21
    ),
    na_values = list(
      c(1, "Diagnostic testing coverage in each health sector Community health workers")
    ),
    known_values = list(
      # The last possible value in the last column
      c(14, "ACT use among antimalarial treatment in each health sector Informal private", "35 (22, 50)")
    )
  )

  # wmr2022f ####
  lup <- c("Lower", "Point", "Upper")
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2022f <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4F.xlsx"),
    sheet = "data",
    range = "A5:I2386",
    col_names = c(
      "WHO region/Country/area",
      "Year",
      "Population denominator for incidence and mortality rate",
      paste0("Cases_", lup),
      paste0("Deaths_", lup)
    )
  ) |>
    dplyr::mutate(`WHO region/Country/area` = ifelse(`WHO region/Country/area` %in% who_regions,
      toupper(`WHO region/Country/area`),
      `WHO region/Country/area`
    )) |>
    split_who_region(col_region_area = 1)

  ## WMR2022F Assertions: ####
  check_who_dataframe(
    df = wmr2022f,
    rows = 2376,
    cols = 10,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 108,
      `Year` = 22
    ),
    known_values = list(
      # The last possible value in the last column
      c(2376, "Deaths_Upper", "-")
    )
  )

  # wmr2022g ####
  pcf <- c("Presumed", "Confirmed", "Footnotes")
  footnote_dict <- c(
    "4"   = "Figures reported for the public sector include cases detected in the private sector.",
    "5"   = "Figures reported for the public sector include cases detected at the community level and in the private sector.",
    "6"   = "Figures include all imported or non-human malaria cases; none of them being indigenous malaria cases."
  )
  wmr2022g <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4G.xlsx"),
    sheet = "AnnexG",
    range = "A8:N102",
    na = c("", "-"),
    col_names = c(
      "WHO region Country/area",
      "UN population",
      "At risk (low + high)",
      "At risk (high)",
      "Number of people living in active foci",
      "Public sector Presumed",
      "Public sector Presumed Footnote",
      "Public sector Confirmed",
      "Public sector Confirmed Footnote",
      "Private sector Presumed",
      "Private sector Confirmed",
      "BLANK",
      "Community level Presumed",
      "Community level Confirmed"
    )
  ) |>
    dplyr::select(!dplyr::starts_with("BLANK")) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(tidyselect::contains("Footnote"), ~ case_match_dict(as.character(.x), footnote_dict)))

  ## WMR2022G Assertions: ####
  check_who_dataframe(
    df = wmr2022g,
    rows = 90,
    cols = 14,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 90
    ),
    na_values = list(
      c(1, "Number of people living in active foci")
    ),
    known_values = list(
      # The last possible value in the last column
      c(90, "Community level Confirmed", 15)
    )
  )

  # wmr2022h ####
  wmr2022h <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4H.xlsx"),
    sheet = "data",
    range = "A4:N719",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Variable",
      2010:2021
    )
  ) |>
    # Note that EUROPEAN region is less common in other datasets (less malaria)
    split_who_region(col_region_area = 1)

  ## WMR2022H Assertions: ####
  check_who_dataframe(
    df = wmr2022h,
    rows = 710,
    cols = 15,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(5, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(710, "2021", "90")
    )
  )

  # wmr2022i ####
  wmr2022i <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4I.xlsx"),
    sheet = "data",
    range = "A4:N614",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Species",
      2010:2021
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2022I Assertions: ####
  check_who_dataframe(
    df = wmr2022i,
    rows = 605,
    cols = 15,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(2, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(605, "2021", "90")
    )
  )

  # wmr2022j ####
  wmr2022j <- readxl::read_excel(file.path(basePath, "WMR2022_Annex_4J.xlsx"),
    sheet = "data",
    range = "A4:M104",
    na = c("", "-", "–"),
    col_names = c(
      "WHO region/Country/area",
      2010:2021
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2022J Assertions: ####
  check_who_dataframe(
    df = wmr2022j,
    rows = 96,
    cols = 14,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 96
    ),
    na_values = list(
      c(12, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(96, "2021", 0)
    )
  )

  list(
    # Annex 1 - Data sources and methods
    # Annex 2 - Number of ITNs distributed through campaigns in malaria endemic countries, 2020–2022
    wmr2022_2 = wmr2022_2,
    # Annex 3 - Regional profiles
    # > A. WHO African Region
    # a. West Africa
    # b. Central Africa
    # c. Countries with high transmission in east and southern Africa
    # d. Countries with low transmission in east and southern Africa
    # > B. WHO Region of the Americas
    # > C. WHO Eastern Mediterranean Region
    # > D. WHO South-East Asia Region
    # > E. WHO Western Pacific Region
    # Annex 4 - Data tables and methods
    # > A. Policy adoption, 2021
    wmr2022a = wmr2022a,
    # > B. Antimalarial drug policy, 2021
    wmr2022b = wmr2022b,
    # > C. Funding for malaria control, 2019–2021
    wmr2022c = wmr2022c,
    # > D. Commodities distribution and coverage, 2019–2021
    wmr2022d = wmr2022d,
    # > Ea. Household survey results, 2017–2021, compiled through STATcompiler
    wmr2022ea = wmr2022ea,
    # > Eb. Household survey results, 2017–2021, compiled through WHO calculations
    wmr2022eb = wmr2022eb,
    # > F. Population denominator for case incidence and mortality rate, and
    # estimated malaria cases and deaths, 2000–2021
    wmr2022f = wmr2022f,
    # > G. Population denominator for case incidence and mortality rate, and reported
    # malaria cases by place of care, 2021
    wmr2022g = wmr2022g,
    # > H. Reported malaria cases by method of confirmation, 2010–2021
    wmr2022h = wmr2022h,
    # > I. Reported malaria cases by species, 2010–2021
    wmr2022i = wmr2022i,
    # > J. Reported malaria deaths, 2010–2021
    wmr2022j = wmr2022j
    # > K. Methods for Tables A-D-G-H-I-J
  )
}

wmr2022 <- get_wmr2022()

usethis::use_data(wmr2022, overwrite = TRUE)
