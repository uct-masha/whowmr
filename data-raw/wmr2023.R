# The `ensure_annex_dirs_exist` function fetches raw data from the WHO website
# and unzips it to a local directory.
# This file brings the unzipped data from the 2023 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2023 page 123:
# Annex 1 - Data sources and methods
# Annex 2 - Number of ITNs distributed through campaigns in malaria endemic
# countries, 2020–2022
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
# > A. Policy adoption, 2022
# > B. Antimalarial drug policy, 2022
# > C. Funding for malaria control, 2020–2022
# > D. Commodities distribution and coverage, 2020–2022
# > E. Household survey results, 2018–2022
# a. Compiled through STATcompiler
# b. Compiled through WHO calculations
# > F. Population denominator for case incidence and mortality rate, and estimated
# malaria cases and deaths, 2000–2022
# > G. Population denominator for case incidence and mortality rate, and reported
# malaria cases by place of care, 2022
# > H. Reported malaria cases by method of confirmation, 2010–2022
# > I. Reported malaria cases by species, 2010–2022
# > J. Reported malaria deaths, 2010–2022
# > K. Methods for Tables A-D-G-H-I-J

get_wmr2023 <- function() {
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2023)
  basePath <- file.path(names(fileTree)[[1]], "WMR2023_Annex")

  # wmr2023_2 ####
  wmr2023_2 <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_2.xlsx"),
    sheet = "Annex 2",
    range = "A5:P69",
    na = "NA"
  ) |>
    dplyr::rename(Country = 1)

  check_who_dataframe(
    df = wmr2023_2,
    rows = 64,
    cols = 16,
    unique_values = list(
      `Country` = 64,
      `ITNs planned for distribution in 2020` = 48
    ),
    na_values = list(
      c(2, "Percentage of planned ITNs distributed in 2020")
    ),
    known_values = list(
      # The last possible value in the last column
      c(64, "Percentage of ITNs planned for distribution in 2022 distributed in 2022", 98.1434950813791)
    )
  )

  # wmr2023a ####
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
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2023a <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4A.xlsx"),
    sheet = "data",
    range = "A2:R95"
  ) |>
    dplyr::rename(`WHO region/Country/area` = 1) |>
    dplyr::mutate(`WHO region/Country/area` = ifelse(`WHO region/Country/area` %in% who_regions,
      toupper(`WHO region/Country/area`),
      `WHO region/Country/area`
    )) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(3:19, ~ case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area` != "United Republic of Tanzania3")

  ## WMR2023A Assertions: ####
  check_who_dataframe(
    df = wmr2023a,
    rows = 87,
    cols = 19,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 87
    ),
    known_values = list(
      # The last possible value in the last column
      c(87, "Directly observed treatment with primaquine is undertaken", "Policy exists and has been implemented this year.")
    )
  )

  # wmr2023b ####
  wmr2023b <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4B.xlsx"),
    sheet = "Data-Table-3B",
    range = "A4:F99",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::rename(
      `Uncomplicated unconfirmed` = `Uncomplicated\r\nunconfirmed`,
      `Uncomplicated confirmed` = `Uncomplicated\r\nconfirmed`
    )

  ## WMR2023B Assertions: ####
  check_who_dataframe(
    df = wmr2023b,
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

  # wmr2023c ####
  wmr2023c <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4C.xlsx"),
    sheet = "Annex_C",
    range = "A6:P289",
    na = c("", "–", "-"),
    col_names = c(
      "WHO region\nCountry/Area",
      "Year",
      "Donor_Global Fund",
      "Donor_PMI/USAID",
      "Donor_The World Bank",
      "Donor_UK",
      "Donor_Others",
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
    )

  ## WMR2023C Assertions: ####
  check_who_dataframe(
    df = wmr2023c,
    rows = 279,
    cols = 17,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 93
    ),
    na_values = list(
      c(1, "Donor_Global Fund")
    ),
    known_values = list(
      # The last possible value in the last column
      c(276, "Country_Other contributions", 0)
    )
  )

  # wmr2023d ####
  wmr2023d <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4D.xlsx"),
    sheet = "Annex_D",
    range = "A4:J279",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2023D Assertions: ####
  check_who_dataframe(
    df = wmr2023d,
    rows = 270,
    cols = 11,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 90
    ),
    na_values = list(
      c(8, "No. of LLINs delivered")
    ),
    known_values = list(
      # The last possible value in the last column
      c(270, "No. of malaria cases treated with ACT", 271)
    )
  )

  # wmr2023e ####
  wmr2023ea <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4Ea.xlsx"),
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

  ## WMR2023EA Assertions: ####
  check_who_dataframe(
    df = wmr2023ea,
    rows = 31,
    cols = 21,
    unique_values = list(
      `WHO Region` = 3,
      `Country/area` = 24
    ),
    na_values = list(
      c(28, "% of households with at least one ITN")
    ),
    known_values = list(
      # The last possible value in the last column
      c(30, "% of children <5 years with fever in last 2 weeks who took an ACT among those who received any antimalarial", 6)
    )
  )

  # wmr2023eb ####
  wmr2023eb <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4Eb.xlsx"),
    sheet = "Annex Eb",
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

  ## WMR2023EB Assertions: ####
  check_who_dataframe(
    df = wmr2023eb,
    rows = 21,
    cols = 27,
    unique_values = list(
      `WHO Region` = 1,
      `Country/area` = 19
    ),
    na_values = list(
      c(20, "Survey")
    ),
    known_values = list(
      # The last possible value in the last column
      c(14, "ACT use among antimalarial treatment in each health sector Informal private", "35 (22, 50)")
    )
  )

  # wmr2023f ####
  lup <- c("Lower", "Point", "Upper")
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2023f <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4F.xlsx"),
    sheet = "Annex_F",
    range = "A8:I2497",
    na = "-",
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

  ## WMR2023F Assertions: ####
  check_who_dataframe(
    df = wmr2023f,
    rows = 2484,
    cols = 10,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 108,
      `Year` = 23
    ),
    na_values = list(
      c(1, "Cases_Lower")
    ),
    known_values = list(
      # The last possible value in the last column
      c(2479, "Deaths_Upper", 11)
    )
  )

  # wmr2023g ####
  pcf <- c("Presumed", "Confirmed", "Footnotes")
  footnote_dict <- c(
    "3" = "Figures reported for the public sector include cases detected in the private sector.",
    "4" = "Figures reported for the public sector include cases detected at the community level.",
    "5" = "Figures reported for the public sector include cases detected at the community level and in the private sector."
  )
  wmr2023g <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4G.xlsx"),
    sheet = "Annex_G",
    range = "A8:N100",
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

  ## WMR2023G Assertions: ####
  check_who_dataframe(
    df = wmr2023g,
    rows = 88,
    cols = 14,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 88
    ),
    na_values = list(
      c(1, "Number of people living in active foci")
    ),
    known_values = list(
      # The last possible value in the last column
      c(88, "Community level Confirmed", 291)
    )
  )

  # wmr2023h ####
  wmr2023h <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4H.xlsx"),
    sheet = "Annex_H",
    range = "A4:O801",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Variable",
      2010:2022
    )
  ) |>
    # Note that EUROPEAN region is less common in other datasets (less malaria)
    split_who_region(col_region_area = 1)

  ## WMR2023H Assertions: ####
  check_who_dataframe(
    df = wmr2023h,
    rows = 792,
    cols = 16,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(1, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(792, "2022", "43")
    )
  )

  # wmr2023i ####
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2023i <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4I.xlsx"),
    sheet = "Annex_I",
    range = "A4:O591",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Species",
      2010:2022
    )
  ) |>
    dplyr::mutate(`WHO region/Country/area` = ifelse(`WHO region/Country/area` %in% who_regions,
      toupper(`WHO region/Country/area`),
      `WHO region/Country/area`
    )) |>
    split_who_region(col_region_area = 1)

  ## WMR2023I Assertions: ####
  check_who_dataframe(
    df = wmr2023i,
    rows = 582,
    cols = 16,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(2, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(582, "2022", "43")
    )
  )

  # wmr2023j ####
  wmr2023j <- readxl::read_excel(file.path(basePath, "WMR2023_Annex_4J.xlsx"),
    sheet = "Annex_J",
    range = "A4:N103",
    na = c("", "-", "–"),
    col_names = c(
      "WHO region/Country/area",
      2010:2022
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2021J Assertions: ####
  check_who_dataframe(
    df = wmr2023j,
    rows = 95,
    cols = 15,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 95
    ),
    na_values = list(
      c(12, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(95, "2022", 0)
    )
  )

  list(
    # Annex 2 - Number of ITNs distributed through campaigns in malaria endemic
    # countries, 2020–2022
    wmr2023_2 = wmr2023_2,
    # Annex 4 - Data tables and methods
    # > A. Policy adoption, 2022
    wmr2023a = wmr2023a,
    # > B. Antimalarial drug policy, 2022
    wmr2023b = wmr2023b,
    # > C. Funding for malaria control, 2020–2022
    wmr2023c = wmr2023c,
    # > D. Commodities distribution and coverage, 2020–2022
    wmr2023d = wmr2023d,
    # > E. Household survey results, 2018–2022
    # a. Compiled through STATcompiler
    wmr2023ea = wmr2023ea,
    # b. Compiled through WHO calculations
    wmr2023eb = wmr2023eb,
    # > F. Population denominator for case incidence and mortality rate, and estimated
    # malaria cases and deaths, 2000–2022
    wmr2023f = wmr2023f,
    # > G. Population denominator for case incidence and mortality rate, and reported
    # malaria cases by place of care, 2022
    wmr2023g = wmr2023g,
    # > H. Reported malaria cases by method of confirmation, 2010–2022
    wmr2023h = wmr2023h,
    # > I. Reported malaria cases by species, 2010–2022
    wmr2023i = wmr2023i,
    # > J. Reported malaria deaths, 2010–2022
    wmr2023j = wmr2023j
  )
}

wmr2023 <- get_wmr2023()

usethis::use_data(wmr2023, overwrite = TRUE)
