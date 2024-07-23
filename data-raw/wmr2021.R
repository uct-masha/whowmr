# The `ensure_annex_dirs_exist` function fetches raw data from the WHO website
# and unzips it to a local directory.
# This file brings the unzipped data from the 2021 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2021 page 111:
# Annex 1 - Data sources and methods
# Annex 2 - Number of ITNs distributed through campaigns in malaria
# endemic countries, 2020–2021
# Annex 3 - High burden to high impact country self-evaluations
# Annex 4 - Regional profiles
# > A. WHO African Region
# a. West Africa
# b. Central Africa
# c. Countries with high transmission in east and southern Africa
# d. Countries with low transmission in east and southern Africa
# > B. WHO Region of the Americas
# > C. WHO Eastern Mediterranean Region
# > D. WHO South-East Asia Region
# > E. WHO Western Pacific Region
# Annex 5 - Data tables and methods
# > A. Policy adoption, 2020
# > B. Antimalarial drug policy, 2020
# > C. Funding for malaria control, 2018–2020
# > D. Commodities distribution and coverage, 2018–2020
# > Ea. Household survey results, 2016–2020, compiled through STATcompiler
# > Eb. Household survey results, 2016–2020, compiled through WHO calculations
# > F. Population denominator for case incidence and mortality rate, and
# estimated malaria cases and deaths, 2000–2020
# > G. Population denominator for case incidence and mortality rate, and reported
# malaria cases by place of care, 2020
# > H. Reported malaria cases by method of confirmation, 2010–2020
# > I. Reported malaria cases by species, 2010–2020
# > J. Reported malaria deaths, 2010–2020
# > K. Methods for Tables A-D-G-H-I-J

get_wmr2021 <- function() {
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2021)
  basePath <- names(fileTree)[[1]]

  # wmr2021a ####
  policy_dict <- c(
    "Y1" = "Policy exists and has been implemented this year.",
    "Y" = "Policy exists but is not implemented this year or no data exists to support implementation.",
    "N" = "Policy does not exist.",
    "D" = "Policy discontinued.",
    "B" = "Both microscopy and RDT are free.",
    "Y2" = "Diagnosis is free but disgnostic test was not specified.",
    "MIC" = "Only microscopy is free.",
    "RDT" = "Only RDT is free.",
    "Ni" = "Neither microscopy nor RDT are free",
    "NA" = "Policy not applicable.",
    "-" = "Question not answered and there is no information from previous years."
  )
  wmr2021a <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5A.xlsx"),
    sheet = "DataTable-3A",
    range = "A2:R99"
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(3:19, ~ case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area` != "United Republic of Tanzania3")

  ## WMR2021A Assertions: ####
  check_who_dataframe(
    df = wmr2021a,
    rows = 91,
    cols = 19,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 91
    ),
    known_values = list(
      # The last possible value in the last column
      c(91, "Directly observed treatment with primaquine is undertaken", "Policy exists and has been implemented this year.")
    )
  )

  # wmr2021b ####
  wmr2021b <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5B.xlsx"),
    sheet = "Data-Table-3B",
    range = "A4:F100",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::rename(
      `Uncomplicated unconfirmed` = `Uncomplicated\r\nunconfirmed`,
      `Uncomplicated confirmed` = `Uncomplicated\r\nconfirmed`
    )

  ## WMR2021B Assertions: ####
  check_who_dataframe(
    df = wmr2021b,
    rows = 91,
    cols = 7,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 91
    ),
    na_values = list(
      c(3, "Uncomplicated unconfirmed")
    ),
    known_values = list(
      # The last possible value in the last column
      c(91, "Treatment", "CQ+PQ")
    )
  )

  # wmr2021c ####
  wmr2021c <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5C.xlsx"),
    sheet = "Funding",
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

  ## WMR2021C Assertions: ####
  check_who_dataframe(
    df = wmr2021c,
    rows = 267,
    cols = 16,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 89
    ),
    na_values = list(
      c(124, "Donor_Global Fund")
    ),
    known_values = list(
      # The last possible value in the last column
      c(267, "Country_Other contributions", 858369)
    )
  )

  # wmr2021d ####
  wmr2021d <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5D.xlsx"),
    sheet = "Table D",
    range = "A4:J294",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2021D Assertions: ####
  check_who_dataframe(
    df = wmr2021d,
    rows = 285,
    cols = 11,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 95
    ),
    na_values = list(
      c(1, "No. of LLINs sold or delivered")
    ),
    known_values = list(
      # The last possible value in the last column
      c(285, "No. of malaria cases treated with ACT", 818)
    )
  )

  # wmr2021e ####
  wmr2021ea <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5-Ea.xlsx"),
    sheet = "annex_3e",
    range = "A6:T51",
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

  ## WMR2021EA Assertions: ####
  check_who_dataframe(
    df = wmr2021ea,
    rows = 41,
    cols = 21,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 31
    ),
    known_values = list(
      # The last possible value in the last column
      c(41, "% of children <5 years with fever in last 2 weeks who took an ACT among those who received any antimalarial", "3.3")
    )
  )

  # wmr2021eb ####
  wmr2021eb <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5-Eb.xlsx"),
    sheet = "Sheet1",
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

  ## WMR2021EB Assertions: ####
  check_who_dataframe(
    df = wmr2021eb,
    rows = 21,
    cols = 27,
    unique_values = list(
      `WHO Region` = 1,
      `Country/area` = 21
    ),
    na_values = list(
      c(17, "Diagnostic testing coverage in each health sector Public excluding community health workers")
    ),
    known_values = list(
      # The last possible value in the last column
      c(13, "ACT use among antimalarial treatment in each health sector Informal private", "35 (22, 50)")
    )
  )

  # wmr2021f ####
  lup <- c("Lower", "Point", "Upper")
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2021f <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5F.xlsx"),
    sheet = "Annex F_2021_10_29",
    range = "A5:I2426",
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

  ## WMR2021F Assertions: ####
  #
  check_who_dataframe(
    df = wmr2021f,
    rows = 2289,
    cols = 10,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 109,
      `Year` = 21
    ),
    known_values = list(
      # The last possible value in the last column
      c(2289, "Deaths_Upper", "765000")
    )
  )

  # wmr2021g ####
  pcf <- c("Presumed", "Confirmed", "Footnotes")
  footnote_dict <- c(
    "4"   = "Figures reported for the public sector include cases detected in the private sector.",
    "5"   = "Figures reported for the public sector include cases detected at the community level and in the private sector.",
    "6"   = "Figures include all imported or non-human malaria cases; none of them being indigenous malaria cases."
  )
  wmr2021g <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5G.xlsx"),
    sheet = "Sheet1",
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

  ## WMR2021G Assertions: ####
  check_who_dataframe(
    df = wmr2021g,
    rows = 92,
    cols = 14,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 90
    ),
    na_values = list(
      c(91, "UN population")
    ),
    known_values = list(
      # The last possible value in the last column
      c(90, "Community level Confirmed", 202)
    )
  )

  # wmr2021h ####
  wmr2021h <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5H.xlsx"),
    sheet = "Sheet1",
    range = "A4:M694",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Variable",
      2010:2020
    )
  ) |>
    # Note that EUROPEAN region is less common in other datasets (less malaria)
    split_who_region(col_region_area = 1)

  ## WMR2021H Assertions: ####
  check_who_dataframe(
    df = wmr2021h,
    rows = 685,
    cols = 14,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(5, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(685, "2020", "46")
    )
  )

  # wmr2021i ####
  wmr2021i <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5I.xlsx"),
    sheet = "data",
    range = "A5:M617",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Species",
      2010:2020
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2021I Assertions: ####
  check_who_dataframe(
    df = wmr2021i,
    rows = 607,
    cols = 14,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(2, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(607, "2020", "46")
    )
  )

  # wmr2021j ####
  wmr2021j <- readxl::read_excel(file.path(basePath, "WMR2021_Annex5J.xlsx"),
    sheet = "_data_table_h",
    range = "A4:V113",
    na = c("", "-", "–"),
    col_names = c(
      "WHO region/Country/area",
      2000:2020
    )
  ) |>
    split_who_region(col_region_area = 1) |>
    # The report didn't display 2000:2009 so remove them:
    dplyr::select(!dplyr::starts_with("200"))

  ## WMR2021J Assertions: ####
  check_who_dataframe(
    df = wmr2021j,
    rows = 104,
    cols = 13,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(12, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(104, "2020", 0)
    )
  )

  list(
    # Annex 5 - Data tables and methods
    # > A. Policy adoption, 2020
    wmr2021a = wmr2021a,
    # > B. Antimalarial drug policy, 2020
    wmr2021b = wmr2021b,
    # > C. Funding for malaria control, 2018–2020
    wmr2021c = wmr2021c,
    # > D. Commodities distribution and coverage, 2018–2020
    wmr2021d = wmr2021d,
    # > Ea. Household survey results, 2016–2020, compiled through STATcompiler
    wmr2021ea = wmr2021ea,
    # > Eb. Household survey results, 2016–2020, compiled through WHO calculations
    wmr2021eb = wmr2021eb,
    # > F. Population denominator for case incidence and mortality rate, and
    # estimated malaria cases and deaths, 2000–2020
    wmr2021f = wmr2021f,
    # > G. Population denominator for case incidence and mortality rate, and reported
    # malaria cases by place of care, 2020
    wmr2021g = wmr2021g,
    # > H. Reported malaria cases by method of confirmation, 2010–2020
    wmr2021h = wmr2021h,
    # > I. Reported malaria cases by species, 2010–2020
    wmr2021i = wmr2021i,
    # > J. Reported malaria deaths, 2010–2020
    wmr2021j = wmr2021j
  )
}

wmr2021 <- get_wmr2021()

usethis::use_data(wmr2021, overwrite = TRUE)
