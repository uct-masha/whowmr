# The `ensure_annex_dirs_exist` function fetches raw data from the WHO website
# and unzips it to a local directory.
# This file brings the unzipped data from the 2024 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2024 page 135:
# Annex 1 - Data sources and methods
# Annex 2 - Number of ITNs distributed through campaigns in malaria endemic
# countries, 2021–2023
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
# > A. Policy adoption, 2023
# > B. Antimalarial drug policy, 2023
# > C. Funding for malaria control, 2021–2023
# > D. Commodities distribution and coverage, 2021–2023
# > E. Household survey results, 2017–2023
# a. Compiled through STATcompiler
# b. Compiled through WHO calculations
# > F. Population denominator for case incidence and mortality rate, and estimated
# malaria cases and deaths, 2000–2023
# > G. Population denominator for case incidence and mortality rate, and reported
# malaria cases by place of care, 2023
# > H. Reported malaria cases by method of confirmation, 2015–2023
# > I. Reported malaria cases by species, 2015–2023
# > J. Reported malaria deaths, 2015–2023
# > K. Malaria endemic countries and areas
# > L. Countries and territories certified malaria free by WHO (1955–2024) and countries
# where malaria never existed or disappeared without specific measures
# > M. Methods for Tables A, D, G, H, I and J


get_wmr2024 <- function() {
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2024)
  basePath <- file.path(names(fileTree)[[1]], "wmr2024-annexes")

  # wmr2024_2 ####
  wmr2024_2 <- readxl::read_excel(file.path(basePath, "wmr2024_annex_2.xlsx"),
    sheet = "Annex2",
    range = "A5:Q57",
    na = "NA"
  ) |>
    dplyr::rename(Country = 1)

  check_who_dataframe(df = wmr2024_2,
                      rows = 52,
                      cols = 17,
                      unique_values = list(
                        `Country` = 52,
                        `ITNs planned for distribution in 2021 (including carry-over from 2020)` = 32
                      ),
                      na_values = list(
                        c(22, "ITNs remaining for distribution in 2022")
                      ),
                      known_values = list(
                        # The last possible value in the last column
                        c(52, "Percentage of ITNs planned for distribution in 2023 distributed in 2023", 37.3438759592533)
                      )
  )

  # wmr2024a ####
  policy_dict <- c(
    "Y1" = "Policy exists and has been implemented this year.",
    "Y" = "Policy exists but is not implemented this year or no data exists to support implementation.",
    "N" = "Policy does not exist.",
    "D" = "Policy discontinued.",
    "B" = "Both microscopy and RDTs are free.",
    "Y2" = "Diagnosis is free but diagnostic test was not specified.",
    "RDT" = "Only RDT is free.",
    "Ni" = "Neither microscopy nor RDT are free.",
    "NA" = "Policy not applicable.",
    "–" = "Question not answered and there is no information from previous years."
  )
  who_regions <- c("AFRICAN", "AMERICAS", "EASTERN MEDITERRANEAN", "EUROPEAN", "SOUTH-EAST ASIA", "WESTERN PACIFIC")
  wmr2024a <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4a.xlsx"),
                     sheet = "Annex_4A",
                     range = "A3:S93"
  ) |>
    dplyr::rename(`WHO region/Country/area` = 2) |>
    dplyr::mutate(`WHO region/Country/area` = ifelse(`WHO region/Country/area` %in% who_regions,
                                                     toupper(`WHO region/Country/area`),
                                                     `WHO region/Country/area`
    )) |>
    split_who_region(col_region_area = 2) |> dplyr::select(!WHO_region) |>
    dplyr::mutate(dplyr::across(3:19, ~ case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area` != "United Republic of Tanzania6")

  ## WMR2024A Assertions: ####
  check_who_dataframe(df = wmr2024a,
                      rows = 84,
                      cols = 19,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 84
                      ),
                      known_values = list(
                        # Dash should not be interpreted as NULL
                        c(9, "Malaria diagnosis is free of charge in the private sector", "Question not answered and there is no information from previous years."),
                        # The last possible value in the last column
                        c(84, "Directly observed treatment with primaquine is undertaken", "Policy exists but is not implemented this year or no data exists to support implementation.")
                      )
  )

  # wmr2024b ####
  wmr2024b <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4b.xlsx"),
    sheet = "Annex_4B",
    range = "A4:F94",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::rename(
      `Uncomplicated unconfirmed` = `Uncomplicated\r\nunconfirmed`,
      `Uncomplicated confirmed` = `Uncomplicated\r\nconfirmed`
    )


  ## WMR2024B Assertions: ####
  check_who_dataframe(
    df = wmr2024b,
    rows = 85,
    cols = 7,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 85
    ),
    na_values = list(
      c(41, "Uncomplicated unconfirmed")
    ),
    known_values = list(
      # The last possible value in the last column
      c(85, "Treatment", "CQ+PQ")
    )
  )

  # wmr2024c ####
  # When converting numbers we found incorrect char used for negative symbol. The analysis looked like this:
  # get violations with:
  # wmr2024c |> tibble::rowid_to_column() |> dplyr::filter(stringr::str_detect(`Donor_Global Fund`,pattern='^\\d',negate=TRUE)) |> dplyr::pull(rowid)
  # get (unique) violating chars to be handled later with:
  # wmr2024c[c(5,87,91,93,107,110,160,197),4] |> purrr::map_chr(~stringr::str_extract(.x,'^.')[[1]]) |> unique()
  wmr2024c <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4c.xlsx"),
    sheet = "Annex_4C",
    range = "A5:Q288",
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
      # next column holds footnotes on how to interpret Government (NMP) column
      "Country_Government Footnotes",
      "Country_Global Fund",
      "Country_Global Fund Footnotes",
      "Country_PMI/USAID",
      "Country_The World Bank",
      "Country_Other bilaterals",
      "Country_WHO",
      "Country_UNICEF",
      "Country_Other contributions"
    )
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(
      `Donor_Global Fund` = `Donor_Global Fund` |> stringr::str_replace_all("–", "-") |> stringr::str_remove_all(' ') |> as.numeric(),
      `Donor_UK` = `Donor_UK`                   |> stringr::str_replace_all("–", "-") |> stringr::str_remove_all(' ') |> as.numeric()
    ) |>
    dplyr::mutate(
      # Translate the footnotes into a boolean column
      `Country_Government Footnotes` = case_match_dict(`Country_Government Footnotes`, dict = c(
        "6" = "Budget, not expenditure.",
        "7" = "WHO NMP funding estimates."
      )),
      `Country_Global Fund Footnotes` = case_match_dict(`Country_Global Fund Footnotes`, dict = c(
        "9" = "Annual disbursements to Yemen from the malaria component of the Global Fund MER Initiative are included in 2021-2023."
      ))
    )

  ## WMR2024C Assertions: ####
  check_who_dataframe(df = wmr2024c,
                      rows = 279,
                      cols = 18,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 93
                      ),
                      na_values = list(
                        c(1, "Donor_Global Fund")
                      ),
                      known_values = list(
                        # Negative values in Donor GlobalFund and UK cols:
                        c(197, "Donor_Global Fund", -627376),
                        c(114, "Donor_UK", -3675),
                        # The last possible value in the last column
                        c(279, "Country_Other contributions", 20000)
                      )
  )

  # wmr2024d ####
  wmr2024d <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4d.xlsx"),
    sheet = "Annex_4D",
    range = "A4:J264",
    na = c("", "–")
  ) |>
    split_who_region(col_region_area = 1)

  # Note: eg, dollar symbol used inline denotes footnote.
  #       We chose not to handle this.

  ## WMR2023D Assertions: ####
  check_who_dataframe(
    df = wmr2024d,
    rows = 255,  # it was 270 last year
    cols = 11,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 85  # it was 90 last year
    ),
    na_values = list(
      c(3, "No. of ITNs delivered")
    ),
    known_values = list(
      # Keep footnotes
      c(2, "Any first-line treatment courses delivered (including ACT)", "4 250 515$"),
      # The last possible value in the last column
      c(255, "No. of malaria cases treated with ACT", 124)
    )
  )

  # wmr2024e ####
  wmr2024ea <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4ea.xlsx"),
    sheet = "Annex_4Ea",
    range = "A5:T26", # African regions only this year so fewer rows. Also note no inclusion of 2017-18 MIS for Burkina Faso (last year included it)
    na = "–",
    col_names = c(
      "WHO Region/Country/area", "Source",
      paste("% of households with", c(
        "at least one ITN",
        "at least one ITN for every two persons who stayed in the household the previous night",
        "IRS in the past 12 months",
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

  ## WMR2024EA Assertions: ####
  check_who_dataframe(df = wmr2024ea,
                      rows = 21,
                      cols = 21,
                      unique_values = list(
                        `WHO Region` = 1,  # last year was 3
                        `Country/area` = 21
                      ),
                      na_values = list(
                        c(2, "% of households with IRS in the past 12 months")
                      ),
                      known_values = list(
                        # Zambia is last row
                        c(21, "Country/area", "Zambia"),
                        # The last possible value in the last column
                        c(21, "% of children <5 years with fever in last 2 weeks who took an ACT among those who received any antimalarial", 96.9)
                      )
  )

  # wmr2024eb ####
  wmr2024eb <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4eb.xlsx"),
    sheet = "Annex_4Eb",
    range = "A6:Z30",
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

  ## WMR2024EB Assertions: ####
  check_who_dataframe(
    df = wmr2024eb,
    rows = 24,
    cols = 27,
    unique_values = list(
      `WHO Region` = 1,
      `Country/area` = 24
    ),
    na_values = list(
      c(1, "Diagnostic testing coverage in each health sector Community health workers")
    ),
    known_values = list(
      # The last possible value in the last column
      c(23, "ACT use among antimalarial treatment in each health sector Informal private", "97 (78, 100)")
    )
  )

  # wmr2024f ####
  lup <- c("Lower", "Point", "Upper")
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2024f <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4f.xlsx"),
    sheet = "Annex4F_allyears",
    range = "A4:I2600",
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
    tibble::add_row(`WHO region/Country/area` = "AFRICAN", .before = 1) |>
    split_who_region(col_region_area = 1)

  ## WMR2024F Assertions: ####
  check_who_dataframe(df = wmr2024f,
                      rows = 2592,
                      cols = 10,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 108,
                        `Year` = 24
                      ),
                      na_values = list(
                        c(1, "Cases_Lower"),
                        c(1538, "Deaths_Lower")
                      ),
                      known_values = list(
                        # The last possible value in the last column
                        c(2586, "Deaths_Upper", 11)
                      )
  )

  # wmr2024g ####
  pcf <- c("Presumed", "Confirmed", "Footnotes")
  footnote_dict <- c(
    "4,6" = "Presumed cases are calculated as: test positivity rate × (suspected cases – tested). Figures reported for the public sector include cases detected at the community level and in the private sector.",
    "5,8" = "Figures reported for the public sector include cases detected in the private sector. Confirmed cases are corrected for double counting of microscopy and RDTs.",
    "6,8" = "Figures reported for the public sector include cases detected at the community level and in the private sector. Confirmed cases are corrected for double counting of microscopy and RDTs.",
    "5" = "Figures reported for the public sector include cases detected in the private sector.",
    "6" = "Figures reported for the public sector include cases detected at the community level and in the private sector.",
    "7" = "Figures reported for the public sector include cases detected at the community level.",
    "8" = "Confirmed cases are corrected for double counting of microscopy and RDTs."
  )

  wmr2024g <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4g.xlsx"),
    sheet = "Annex_4G",
    range = "A6:N98",
    na = c("", "–"),
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


  ## WMR2024G Assertions: ####
  check_who_dataframe(df = wmr2024g,
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
                        c(84, "Community level Confirmed", 5685)
                      )
  )

  # wmr2024h ####
  wmr2024h <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4h.xlsx"),
    sheet = "Annex_4H",
    range = "A4:K898",
    na = c("", "–"),
    col_names = c(
      "WHO region/Country/area",
      "Variable",
      2015:2023
    )
  ) |>
    # Note that EUROPEAN region is less common in other datasets (less malaria)
    split_who_region(col_region_area = 1)

  ## WMR2024H Assertions: ####
  check_who_dataframe(df = wmr2024h,
                      rows = 889,
                      cols = 12,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 104
                      ),
                      na_values = list(
                        c(15, "2015")
                      ),
                      known_values = list(
                        # The last possible value in the last column
                        c(889, "2023", "100")
                      )
  )

  # wmr2024i ####
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2024i <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4i.xlsx"),
    sheet = "Annex_4I",
    range = "A4:K465",
    na = c("", "–"),
    col_names = c(
      "WHO region/Country/area",
      "Species",
      2015:2023
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2024I Assertions: ####
  check_who_dataframe(df = wmr2024i,
                      rows = 456,
                      cols = 12,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 104
                      ),
                      na_values = list(
                        c(9, "2015")
                      ),
                      known_values = list(
                        # The last possible value in the last column
                        c(456, "2023", "75")
                      )
  )

  # wmr2024j ####
  wmr2024j <- readxl::read_excel(file.path(basePath, "wmr2024_annex_4j.xlsx"),
    sheet = "Annex_4J",
    range = "A4:J104",
    na = c("", "-", "–"),
    col_names = c(
      "WHO region/Country/area",
      2015:2023
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2024J Assertions: ####
  check_who_dataframe(df = wmr2024j,
                      rows = 96,
                      cols = 11,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 96
                      ),
                      na_values = list(
                        c(30, "2015")
                      ),
                      known_values = list(
                        # The last possible value in the last column
                        c(96, "2023", 0)
                      )
  )

  list(
    # Annex 2 – Number of ITNs distributed through campaigns in malaria endemic
    # countries, 2021–2023
    wmr2024_2 = wmr2024_2,
    # Annex 4 – Data tables and methods
    # A. Policy adoption, 2023
    wmr2024a = wmr2024a,
    # B. Antimalarial drug policy, 2023
    wmr2024b = wmr2024b,
    # C. Funding for malaria control, 2021–2023
    wmr2024c = wmr2024c,
    # D. Commodities distribution and coverage for malaria endemic countries, 2021–2023
    wmr2024d = wmr2024d,
    # E. Household survey results, 2017–2023
    # a. Compiled through STATcompiler for the WHO African Region
    wmr2024ea = wmr2024ea,
    # b. Compiled through WHO calculations for the WHO African Region
    wmr2024eb = wmr2024eb,
    # F. Population denominator for case incidence and mortality rate, and estimated malaria cases
    # and deaths, 2000–2023
    wmr2024f = wmr2024f,
    # G. Population denominator for case incidence and mortality rate, and reported malaria cases
    # by place of care, 2023
    wmr2024g = wmr2024g,
    # H. Reported malaria cases by method of confirmation, 2015–2023
    wmr2024h = wmr2024h,
    # I. Reported malaria cases by species, 2015–2023
    wmr2024i = wmr2024i,
    # J. Reported malaria deaths, 2015–2023
    wmr2024j = wmr2024j
  )
}

wmr2024 <- get_wmr2024()

usethis::use_data(wmr2024, overwrite = TRUE)
