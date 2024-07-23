# The `ensure_annex_dirs_exist` function fetches raw data from the WHO website
# and unzips it to a local directory.
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
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2018)
  basePath <- names(fileTree)[[1]]
  # TODO: Remove footnotes in column names
  # TODO: Remove footnotes in cell values

  # wmr2018a ####
  policy_dict <- c(
    "●" = "Actually implemented",
    "◌" = "Not implemented",
    "-" = "Question not answered or not applicable",
    "NA" = "Not applicable",
    `NA` = NA
  )
  wmr2018a <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-a.xls"),
    sheet = "Sheet1",
    range = "A2:Q102"
  ) |>
    dplyr::mutate(`WHO region\nCountry/area` = dplyr::if_else(`WHO region\nCountry/area` == "Western Pacific",
      toupper(`WHO region\nCountry/area`),
      `WHO region\nCountry/area`
    )) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(3:18, ~ case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area` != "United Republic of Tanzania3")

  ## WMR2018A Assertions: ####
  check_who_dataframe(
    df = wmr2018a,
    rows = 94,
    cols = 18,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 94
    ),
    known_values = list(
      # cell D50 is Zimbabwe and does not distribute ITNs through mass campaigns
      list(46, "ITNs/LLINs distributed through mass campaigns to all age groups", val = "Not implemented"),
      # cell A102 is "Viet Nam" and is in the Western Pacific region
      list(94, "WHO Region", val = "WESTERN PACIFIC"),
      list(94, "Country/area", val = "Viet Nam")
    )
  )

  # wmr2018b ####
  wmr2018b <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-b.xls"),
    sheet = "Sheet1",
    range = "A2:F102"
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::rename(
      `Uncomplicated unconfirmed` = `Uncomplicated\nunconfirmed`,
      `Uncomplicated confirmed` = `Uncomplicated\nconfirmed`
    )

  ## WMR2018B Assertions: ####
  check_who_dataframe(
    df = wmr2018b,
    rows = 95,
    cols = 7,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 95
    ),
    # cell B4 is empty for Algeria which is na
    na_values = list(
      c(1, "Uncomplicated unconfirmed")
    ),
    known_values = list(
      # cell F52 is Argentina in the Americas with CQ+PQ for P.vivax Treatment
      list(48, "WHO Region", val = "AMERICAS"),
      list(48, "Country/area", val = "Argentina"),
      list(48, "Treatment", val = "CQ+PQ")
    )
  )

  # wmr2018c ####
  wmr2018c <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-c.xls"),
    sheet = "Sheet1",
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
    try_make_numeric(
      cols = setdiff(4:16, 9),
      str_remove = "[’']+"
    )

  ## WMR2018C Assertions: ####
  check_who_dataframe(
    df = wmr2018c,
    rows = 279,
    cols = 16,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 93
    ),
    na_values = list(
      list(3, "Country_Government Footnotes"), # H9, Algeria, 2017
      list(138, "Country_WHO") # M144, Zimbabwe, 2017
    ),
    known_values = list(
      # cell G215 is Pakistan 2016 with value "16’400’000"
      list(209, "WHO Region", "EASTERN MEDITERRANEAN"),
      list(209, "Country/area", "Pakistan"),
      list(209, "Country_Government (NMP)", "16400000")
    )
  )

  # wmr2018d ####
  wmr2018d <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-d.xls"),
    sheet = "Sheet1",
    range = "A1:I294",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1) |>
    # column 7 is irs coverage and includes <1 so we impose no parsing to numeric there
    try_make_numeric(setdiff(4:10, 7),
      str_remove = "[-']+"
    )

  ## WMR2018D Assertions: ####
  check_who_dataframe(
    df = wmr2018d,
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
      list(221, "WHO Region", "EASTERN MEDITERRANEAN"),
      list(221, "Country/area", "Somalia"),
      list(221, "No. of LLINs sold or delivered", 655798),
      # # F294 (last row) is <1 IRS cov and 417142 IRS protected
      list(287, "WHO Region", "WESTERN PACIFIC"),
      list(287, "Country/area", "Viet Nam"),
      list(287, 6, 417142),
      list(287, "IRS coverage (%)", "<1")
    )
  )

  # wmr2018e ####
  wmr2018e <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-e.xls"),
    sheet = "Sheet1",
    range = "A8:T35",
    col_names = c(
      "WHO Region/Country/area", "Source",
      paste0("% of households with ", c(
        "at least one ITN",
        "at least one ITN for every two persons who stayed in the household the previous night",
        "IRS in last 12 months",
        "at least one ITN and/or IRS in the past 12 months",
        "at least one ITN for every two persons and/or IRS in the past 12 months"
      )),
      "% of population with access to an ITN",
      "% of population who slept under an ITN last night",
      "% of ITNs that were used last night",
      "% of pregnant women who slept under an ITN",
      "% of pregnant women who took 3+ doses of IPTp at least one during ANC visit",
      paste0(
        "% of children <5 years ", c(
          "who slept under an ITN",
          "with haemoglobin <8 g/dL",
          "with a positive RDT",
          "with a positive microscopy blood smear"
        )
      ),
      paste0("% of children <5 years with fever in last 2 weeks ", c(
        "for whom advice or treatment was sought",
        "who had blood taken from a finger or heel for testing",
        "who took antimalarial drugs",
        "who took an ACT among those who received any antimalarial"
      ))
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2018E Assertions: ####
  check_who_dataframe(
    df = wmr2018e,
    rows = 25,
    cols = 21,
    unique_values = list(
      `WHO Region` = 3,
      `Country/area` = 22
    ),
    na_values = list(
      list(4, "% of children <5 years with fever in last 2 weeks who had blood taken from a finger or heel for testing"), # R12 Ethiopia
      list(23, "% of pregnant women who took 3+ doses of IPTp at least one during ANC visit") # L22, Rwanda
    ),
    known_values = list(
      # second last row is Myanmar with last value in Q34 = 3
      list(24, "WHO Region", "SOUTH-EAST ASIA"),
      list(24, "Country/area", "Myanmar"),
      list(24, "% of children <5 years with fever in last 2 weeks who had blood taken from a finger or heel for testing", 3)
    )
  )

  # wmr2018f ####
  lup <- c("Lower", "Point", "Upper")
  wmr2018f <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-f.xls"),
    sheet = "Sheet1",
    range = "A4:I881",
    col_names = c(
      "WHO region/Country/area",
      "Year",
      "Population at risk",
      paste0("Cases_", lup),
      paste0("Deaths_", lup)
    )
  ) |>
    split_who_region(col_region_area = 1) |>
    try_make_numeric(5:10)

  ## WMR2018F Assertions: ####
  check_who_dataframe(
    df = wmr2018f,
    rows = 872,
    cols = 10,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 152
    ),
    # cell
    na_values = list(
      list(440, "Deaths_Lower") # G445
    ),
    known_values = list(
      # Row 800 is Timor-Leste 2017 data
      list(792, "WHO Region", "SOUTH-EAST ASIA"),
      list(792, "Country/area", "Timor-Leste"),
      list(792, "Deaths_Point", 0),
      list(792, "Cases_Lower", 30)
    )
  )

  # wmr2018g ####
  pc <- c("Presumed", "Confirmed")
  wmr2018g <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-g.xls"),
    sheet = "Sheet1",
    range = "A4:L100",
    na = c("", "-"),
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
    )
  ) |>
    dplyr::select(!dplyr::starts_with("BLANK")) |>
    split_who_region(col_region_area = 1)
  # NOTE: Tanzania is split into Mainland and Zanzibar
  #       When making the summary table do not double count Tanzania

  ## WMR2018G Assertions: ####
  check_who_dataframe(
    df = wmr2018g,
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
      # cell G52 is Belize with UN population 374686
      list(47, "WHO Region", "AMERICAS"),
      list(47, "Country/area", "Belize"),
      list(47, "UN population", 374686)
    )
  )

  # wmr2018h ####
  wmr2018h <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-h.xls"),
    sheet = "Sheet1",
    range = "A2:T631",
    na = "-",
    col_names = c(
      "WHO region/Country/area",
      "Variable",
      2000:2017
    )
  ) |>
    # Explicitly remove the 2000:2010 data not included in the report
    dplyr::select(`WHO region/Country/area`, Variable, `2010`:`2017`) |>
    # Note that EUROPEAN region is less common in other datasets (less malaria)
    split_who_region(col_region_area = 1)

  ## WMR2018H Assertions: ####
  check_who_dataframe(
    df = wmr2018h,
    rows = 624,
    cols = 11,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    # cell I6 is "-" which is na
    na_values = list(
      list(4, "2014"),
      # Note: Cabo Verde 2016 is defined in 2017 data (I42) but not in 2018 data (S42)
      list(40, "2016")
    ),
    known_values = list(
      list(622, "WHO Region", "WESTERN PACIFIC"),
      list(622, "Country/area", "Viet Nam"),
      list(622, "2016", 408055) # S629
    )
  )

  # wmr2018i ####
  wmr2018i <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-i.xls"),
    sheet = "Sheet1",
    range = "A2:T527",
    na = "-",
    col_names = c(
      "WHO region/Country/area",
      "Species",
      2000:2017
    )
  ) |>
    # Explicitly remove the 2000:2010 data not included in the report
    dplyr::select(`WHO region/Country/area`, Species, `2010`:`2017`) |>
    split_who_region(col_region_area = 1)

  ## WMR2018I Assertions: ####
  check_who_dataframe(
    df = wmr2018i,
    rows = 520,
    cols = 11,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    # cell I387 is "-" which is na
    na_values = list(
      list(380, 10)
    ),
    known_values = list(
      # S527 is Viet Nam with 15 "No Other" cases in 2016
      list(520, "WHO Region", "WESTERN PACIFIC"),
      list(520, "Country/area", "Viet Nam"),
      list(520, "2016", 15)
    )
  )

  # wmr2018j ####
  wmr2018j <- readxl::read_excel(file.path(basePath, "wmr2018-annex-table-j.xls"),
    sheet = "Sheet1",
    range = "A2:S111",
    na = "-",
    col_names = c(
      "WHO region/Country/area",
      2000:2017
    )
  ) |>
    # Explicitly remove the 2000:2010 data not included in the report
    dplyr::select(`WHO region/Country/area`, `2010`:`2017`) |>
    split_who_region(col_region_area = 1)

  ## WMR2018J Assertions: ####
  check_who_dataframe(
    df = wmr2018j,
    rows = 104,
    cols = 10,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    # cell
    na_values = list(
      list(70, "2017") # S74, Djibouti
    ),
    known_values = list(
      list(70, "WHO Region", "EASTERN MEDITERRANEAN"),
      list(70, "Country/area", "Djibouti"),
      list(104, "2016", 2) # R111, Viet Nam 2016
    )
  )

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
