# The `ensure_annex_dirs_exist` function fetches raw data from the WHO website
# and unzips it to a local directory.
# This file brings the unzipped data from the 2019 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2019 page 83:
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
# > A. Policy adoption, 2018
# > B. Antimalarial drug policy, 2018
# > C. Funding for malaria control, 2016–2018
# > D. Commodities distribution and coverage, 2016–2018
# > Ea. Household survey results, 2015–2018, compiled through STATcompiler
# > Eb. Household survey results, 2015–2018, compiled through WHO calculations
# > F. Population at risk and estimated malaria cases and deaths, 2010–2018
# > G. Population at risk and reported malaria cases by place of care, 2018
# > H. Reported malaria cases by method of confirmation, 2010–2018
# > I. Reported malaria cases by species, 2010–2018
# > J. Reported malaria deaths, 2010–2018

get_wmr2019 <- function() {
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2019)
  basePath <- names(fileTree)[[1]]

  # wmr2019a ####
  # This was provided as a PDF file so I manually extracted it to excel format.
  # Another innovation was the inclusion of multiple types of answers (not just yes/no)
  # The different types were marked with different symbols/colours in the PDF
  # and I exported PDF this to Excel using Adobe Acrobat. I then moved some
  # columns around and unmerged some cells to make it machine readable.
  # The next challenge was to isolate the red/black/grey circles. For this I
  # initially tried to write a macro in VBA but something wasn't working out for
  # me there so I decided to do it manually. To check I flicked between the sheets
  # but also decided to make screenshots of both sheets and loaded them as layers
  # in GIMP. I used the filters/opacity to confirm my work so I'm pretty confident this is correct.
  # I chose to express the values to factors.
  # Note also that "Malaria diagnosis is free of charge in the public sector" is
  # marked with as * for Burundi and Togo meaning:
  # "Free for children and/or pregnant women only".
  policy_dict <- readxl::read_excel("inst/extdata/wmr2019-annex-table-a.xlsx",
    sheet = "Key"
  ) |>
    dplyr::pull(Meaning, name = "Symbol")
  wmr2019a <- readxl::read_excel("inst/extdata/wmr2019-annex-table-a.xlsx",
    sheet = "ManualExtraction",
    range = "A2:R101"
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(3:19, ~ case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area` != "United Republic of Tanzania3")

  ## WMR2019A Assertions: ####
  check_who_dataframe(
    df = wmr2019a,
    rows = 93,
    cols = 19,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 93
    ),
    known_values = list(
      # cell E50 is Zimbabwe and does distributes ITNs through mass campaigns
      list(46, "ITNs/LLINs are distributed through mass campaigns",
        val = "Policy adopted and implemented this year. Available data from the world malaria report data collection form provides evidence for implementation."
      ),
      # cell A101 is "Viet Nam" and is in the Western Pacific region
      list(93, "WHO Region", val = "WESTERN PACIFIC"),
      list(93, "Country/area", val = "Viet Nam")
    )
  )

  # wmr2019b ####
  wmr2019b <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-b.xls"),
    sheet = "DrugPolicy", # There are hidden sheets
    range = "A4:F105",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::rename(
      `Uncomplicated unconfirmed` = `Uncomplicated\nunconfirmed`,
      `Uncomplicated confirmed` = `Uncomplicated\nconfirmed`
    )

  ## WMR2019B Assertions: ####
  check_who_dataframe(
    df = wmr2019b,
    rows = 96,
    cols = 7,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 96
    ),
    # cell B4 is empty for Algeria which is na
    na_values = list(
      c(1, "Uncomplicated unconfirmed")
    ),
    known_values = list(
      # cell F54 is Argentina in the Americas with CQ+PQ for P.vivax Treatment
      list(48, "WHO Region", val = "AMERICAS"),
      list(48, "Country/area", val = "Argentina"),
      list(48, "Treatment", val = "CQ + PQ")
    )
  )

  # wmr2019c ####
  wmr2019c <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-c.xls"),
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

  ## WMR2019C Assertions: ####
  check_who_dataframe(
    df = wmr2019c,
    rows = 279,
    cols = 16,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 93
    ),
    na_values = list(
      c(1, "Country_Government Footnotes")
    ),
    known_values = list(
      # cell O289 is Viet Nam 2018, the last possible cell
      list(279, "WHO Region", val = "WESTERN PACIFIC"),
      list(279, "Country/area", val = "Viet Nam"),
      list(279, "Year", val = 2018),
      list(279, "Country_Other contributions", val = 315396)
    )
  )

  # wmr2019d ####
  wmr2019d <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-d.xls"),
    sheet = "Table D (Coverage)",
    range = "A4:H297",
    na = c("", "-")
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2019D Assertions: ####
  check_who_dataframe(
    df = wmr2019d,
    rows = 288,
    cols = 9,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 96
    ),
    na_values = list(
      # D6 is the first row, Algeria 2016
      c(1, "Modelled percentage of population with access to an ITN")
    ),
    known_values = list(
      # cell H297 is Viet Nam 2018, the last possible cell
      list(288, "WHO Region", val = "WESTERN PACIFIC"),
      list(288, "Country/area", val = "Viet Nam"),
      list(288, "Year", val = 2018),
      list(288, "ACT treatment courses delivered", val = 40000)
    )
  )

  # wmr2019e ####
  # This 2019 sheet uses some slightly different columns and the user should be aware of this.
  wmr2019ea <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-e-a.xls"),
    sheet = "Sheet1",
    range = "A6:T44",
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

  ## WMR2019EA Assertions: ####
  check_who_dataframe(
    df = wmr2019ea,
    rows = 35,
    cols = 21,
    unique_values = list(
      `WHO Region` = 4,
      `Country/area` = 27
    ),
    na_values = list(
      c(6, "% of households with at least one ITN")
    ),
    known_values = list(
      # The last possible value
      c(35, "% of children <5 years with fever in last 2 weeks who took an ACT among those who received any antimalarial", 11.1)
    )
  )

  # wmr2019eb ####
  wmr2019eb <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-e-b.xls"),
    sheet = "Annex",
    range = "A5:Z25",
    na = c("", "–"),
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

  ## WMR2019EB Assertions: ####
  check_who_dataframe(
    df = wmr2019eb,
    rows = 20,
    cols = 27,
    unique_values = list(
      `WHO Region` = 1, # Just AFRICAN region in this file
      `Country/area` = 20
    ),
    na_values = list(
      c(1, "Health sector where treatment was sought Community health workers")
    ),
    known_values = list(
      # The last possible value
      c(18, "ACT use among antimalarial treatment in each health sector Informal private", 0.960972373820632)
    )
  )


  # wmr2019f ####
  lup <- c("Lower", "Point", "Upper")
  wmr2019f <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-f.xls"),
    sheet = "apendix_f_data",
    range = "A8:J974",
    col_names = c(
      "WHO region", "Country/area",
      "Year",
      "Population at risk",
      paste0("Cases_", lup),
      paste0("Deaths_", lup)
    )
  ) |>
    dplyr::mutate(`WHO region` = toupper(`WHO region`))

  ##  WMR2019F Assertions: ####
  check_who_dataframe(
    df = wmr2019f,
    rows = 967,
    cols = 10,
    unique_values = list(
      `WHO region` = 6,
      `Country` = 108,
      `Year` = 9
    ),
    na_values = list(
      c(1, "Cases_Lower")
    ),
    known_values = list(
      # The last possible value in the last column
      c(967, "Deaths_Upper", 16)
    )
  )

  # wmr2019g ####
  pcf <- c("Presumed", "Confirmed", "Footnotes")
  footnote_dict <- c(
    "2"   = "Where national data for the United Republic of Tanzania are unavailable, refer to Mainland and Zanzibar.",
    "3"   = "Figures reported for the public sector include cases detected at the community level.",
    "4"   = "Figures reported for the public sector include cases detected in the private sector.",
    "5"   = "Figures reported for the public sector include cases detected at the community level and in the private sector.",
    "6"   = "Figures include all imported or non-human malaria cases; none of them being indigenous malaria cases.",
    "5,6" = "Figures reported for the public sector include cases detected at the community level and in the private sector. Figures include all imported or non-human malaria cases; none of them being indigenous malaria cases."
  )
  wmr2019g <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-g.xls"),
    range = "A8:N104",
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
      "BLANK",
      "Community level Confirmed"
    )
  ) |>
    dplyr::select(!dplyr::starts_with("BLANK")) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(tidyselect::contains("Footnote"), ~ case_match_dict(as.character(.x), footnote_dict)))

  ## WMR2019G Assertions: ####
  check_who_dataframe(
    df = wmr2019g,
    rows = 92,
    cols = 14,
    unique_values = list(
      `WHO Region` = 5,
      `Country/area` = 92
    ),
    na_values = list(
      c(1, "At risk (low + high)")
    ),
    known_values = list(
      # G39 has a footnote attached so we can't make it a numeric column
      c(31, "Public sector Confirmed", "36451*"),
      # The last possible value in the last column
      c(91, "Community level Confirmed", 150)
    )
  )

  # wmr2019h ####
  # Note: hidden sheets
  footnote_dict <- c(
    "2" = "In May 2013, South Sudan was reassigned to the WHO African Region (WHA resolution 66.21, https://apps.who.int/gb/ebwha/pdf_files/WHA66/A66_R21-en.pdf).",
    "3" = "Figures reported for the public sector include cases detected at the community level.",
    "4" = "Figures reported for the public sector include cases detected in the private sector.",
    "5" = "Figures reported for the public sector include cases detected at the community level and in the private sector.",
    "5,6" = "Figures reported for the public sector include cases detected at the community level and in the private sector. Figures include all imported or non-human malaria cases; none of them being indigenous malaria cases.",
    "5,8" = "Figures reported for the public sector include cases detected at the community level and in the private sector. Incomplete laboratory data. This country has no presumed cases reported."
  )
  wmr2019h <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-h.xls"),
    sheet = "Table H (CasesByConfirmation)",
    range = "A7:V631",
    na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Variable",
      2000:2018, # Also 1990 data in a hidden sheet.
      "Footnotes"
    )
  ) |>
    # Note that EUROPEAN region is less common in other datasets (less malaria)
    split_who_region(col_region_area = 1) |>
    try_make_numeric(4:12) |>
    dplyr::mutate(Footnotes = case_match_dict(Footnotes, footnote_dict)) |>
    # Hide the 2000:2009 data since it was not used in the report:
    dplyr::select(`WHO Region`, `Country/area`, Variable, tidyselect::starts_with("201"), Footnotes)
  # NOTE: Tanzania, Mainland and Zanzibar are split again
  # I did not check the Regional Summary

  ## WMR2019H Assertions: ####
  check_who_dataframe(
    df = wmr2019h,
    rows = 619,
    cols = 13,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 103
    ),
    na_values = list(
      c(409, "Variable")
    ),
    known_values = list(
      # wmr2019h[187, "2018"] gives "36451*"
      c(187, "2018", "36451*"),
      # The last possible value in the last column
      c(614, "Footnotes", "Figures reported for the public sector include cases detected at the community level and in the private sector.")
    )
  )

  # wmr2019i ####
  wmr2019i <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-3-i.xls"),
    range = "A5:K502", na = c("", "-"),
    col_names = c(
      "WHO region/Country/area",
      "Species",
      2010:2018
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2019I Assertions: ####
  check_who_dataframe(
    df = wmr2019i,
    rows = 492,
    cols = 12,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(2, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(492, "2018", 13)
    )
  )


  # wmr2019j ####
  wmr2019j <- readxl::read_excel(file.path(basePath, "wmr2019-annex-table-j.xls"),
    range = "A4:T113", na = c("", "-", "–"),
    col_names = c(
      "WHO region/Country/area",
      2000:2018
    )
  ) |>
    # Explicitly remove the 2000:2010 data not included in the report
    dplyr::select(`WHO region/Country/area`, `2010`:`2018`) |>
    split_who_region(col_region_area = 1)

  ## WMR2019J Assertions: ####
  check_who_dataframe(
    df = wmr2019j,
    rows = 104,
    cols = 11,
    unique_values = list(
      `WHO Region` = 6,
      `Country/area` = 104
    ),
    na_values = list(
      c(12, "2010")
    ),
    known_values = list(
      # The last possible value in the last column
      c(104, "2018", 1)
    )
  )


  list(
    # Annex 3
    # A. Policy adoption, 2018. Tanzania dropped.
    wmr2019a = wmr2019a,
    # B. Antimalarial drug policy, 2018
    wmr2019b = wmr2019b,
    # C. Funding for malaria control, 2016–2018
    wmr2019c = wmr2019c,
    # D. Commodities distribution and coverage, 2016–2018
    wmr2019d = wmr2019d,
    # Ea. Household survey results, 2015–2018, compiled through STATcompiler
    wmr2019ea = wmr2019ea,
    # Eb. Household survey results, 2015–2018, compiled through WHO calculations
    wmr2019eb = wmr2019eb,
    # F. Population at risk and estimated malaria cases and deaths, 2010–2018
    wmr2019f = wmr2019f,
    # G. Population at risk and reported malaria cases by place of care, 2018
    wmr2019g = wmr2019g,
    # H. Reported malaria cases by method of confirmation, 2010–2018
    wmr2019h = wmr2019h,
    # I. Reported malaria cases by species, 2010–2018
    wmr2019i = wmr2019i,
    # J. Reported malaria deaths, 2010–2018
    wmr2019j = wmr2019j
  )
}

wmr2019 <- get_wmr2019()

usethis::use_data(wmr2019, overwrite = TRUE)
