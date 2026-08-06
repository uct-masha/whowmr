# The `ensure_annex_dirs_exist` function fetches raw data from the WHO website
# and unzips it to a local directory.
# This file brings the unzipped data from the 2025 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2025 page 125:
# Annex 1 - Data sources and methods
# Annex 2 - Number of ITNs distributed through campaigns in malaria endemic
# countries, 2022–2024
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
# > A. Policy adoption, 2024
# > B. Antimalarial drug policy in malaria endemic countries and areas, 2024
# > C. Household survey results, 2017–2024
# a. Compiled through STATcompiler for the WHO African Region
# > D. Malaria endemic countries and areas
# > E. Countries and areas certified malaria free by WHO (1955–2025) and countries
# where malaria never existed or disappeared without specific measures


get_wmr2025 <- function() {
  source("R/utils.R") # stop_if_not_installed
  source("R/ensure_annex_dirs_exist.R")
  source("R/case_match_dict.R")
  source("R/split_who_region.R")
  source("R/try_make_numeric.R")
  source("R/check_who_dataframe.R")
  stop_if_not_installed("readxl", "dplyr")
  # First we need to ensure the unzipped annex file exists:
  fileTree <- ensure_annex_dirs_exist(2025)
  basePath <- file.path(names(fileTree)[[1]], "wmr2025-annexes")

  # wmr2025_2 ####
  wmr2025_2 <- readxl::read_excel(file.path(basePath, "wmr2025_annex_2.xlsx"),
    sheet = "Annex2Final",
    range = "A5:Q60",
    na = "NA"
  ) |>
    dplyr::rename(Country = 1) |>
    # We have a row for "United Republic of Tanzania" with no values
    # Then a row for Mainland5,6 with values and Zanzibar5,6 with values.
    # We want just the latter 2 but Mainland should be `United Republic of Tanzania (Mainland)`
    dplyr::mutate(Country = dplyr::case_match(Country,
      "Mainland5,6" ~ "United Republic of Tanzania (Mainland)5,6",
      "Zanzibar5,6" ~ "Zanzibar5,6",
      .default = Country
    )) |>
    dplyr::filter(Country != "United Republic of Tanzania")
  
  check_who_dataframe(df = wmr2025_2,
                        rows = 54,
                        cols = 17,
                        unique_values = list(
                          `Country` = 54,
                          `ITNs planned for distribution in 2022 (including carry-over from 2021)` = 37
                        ),
                        na_values = list(
                          c(2, "Percentage of remaining ITNs from 2022 distributed in 2023 (including carry-over from 2022)"),
                          c(4, "Percentage of ITNs planned for distribution in 2022 and distributed in 2022")
                        ),
                        known_values = list(
                          # The first possible value in first nontrivial column
                          c(1, "ITNs planned for distribution in 2022 (including carry-over from 2021)",  2195198),
                          # The last possible value in the last column
                          c(54, "Percentage of ITNs planned for distribution in 2024 and distributed in 2024", 100)
                        )
    )

  # wmr2025a ####
  # -- FROM THE SHEET: --
  # ACT: artemisinin-based combination therapy; c-IPTp: Community based delivery of IPTp; IPTp: intermittent preventive treatment of malaria in pregnancy; IPTsc: Intermittent Preventive Treatment of malaria in school-aged children; IRS: indoor residual spraying; ITN: insecticide-treated mosquito net; NMP: national malaria programme; P.: Plasmodium; PMC: perennial malaria chemoprevention; RDT: rapid diagnostic test; SMC: seasonal malaria chemoprevention; WHO: World Health Organization.																
  # 1 Single dose of primaquine (0.75 mg base/kg) for countries in the WHO Region of the Americas.																
  # 2 Mass campaigns do not include targeted campaigns directed towards specific sub-groups in the population.																
  # 3 Data reported to the Alliance for Malaria Prevention were used where data reported to WHO were missing or incomplete.																
  # 4 In May 2013, South Sudan was reassigned to the WHO African Region (resolution WHA66.21, https://apps.who.int/gb/ebwha/pdf_files/WHA66/A66_R21-en.pdf).																
  # 5 Where national data for the United Republic of Tanzania are unavailable, refer to Mainland and Zanzibar.																
  # 6 As of 27 May 2025, Indonesia was reassigned to the WHO Western Pacific Region (resolution WHA78.25, https://apps.who.int/gb/ebwha/pdf_files/WHA78/A78_R25-en.pdf).
  
  policy_dict <- c(
    "Y1" = "Policy exists and is implemented this year.",
    "Y" = "Policy exists but is not implemented this year or no data exists to support implementation.",
    "N" = "Policy does not exist or policy has been discontinued.",
    "NA" = "Policy not applicable.",
    "-" = "Question not answered and there is no information from previous years."
  )

  wmr2025a <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4a.xlsx"),
                                 sheet = "Annex A",
                                 range = "A4:S91"
  ) |>
    dplyr::rename(`WHO region/Country/area` = 1) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(3:20, ~ case_match_dict(.x, policy_dict))) |>
    dplyr::filter(`Country/area` != "United Republic of Tanzania5")

  ## WMR2025A Assertions: ####
  check_who_dataframe(df = wmr2025a,
                      rows = 81,
                      cols = 20,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 81
                      ),
                      known_values = list(
                        # The first possible value in the first nontrivial column
                        c(1, "Malaria diagnosis with microscopy or RDT is free in public sector", policy_dict[["Y1"]]),
                        # The last possible value in the last column
                        c(81, "Insecticide resistance is monitored and reported to WHO", policy_dict[["Y"]])
                      )
  )

  # wmr2025b ####
  wmr2025b <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4b.xlsx"),
    sheet = "Annex B",
    range = "A4:F91",
    na = c("")
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::rename(
      `Uncomplicated unconfirmed` = `Uncomplicated\r\nunconfirmed`,
      `Uncomplicated confirmed` = `Uncomplicated\r\nconfirmed`
    )
  
  ## WMR2025B Assertions: ####
  check_who_dataframe(df = wmr2025b,
                      rows = 82,
                      cols = 7,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 82
                      ),
                      na_values = list(
                        c(40, "Uncomplicated unconfirmed")
                      ),
                      known_values = list(
                        # The last possible value in the last column
                        c(82, "Treatment", "CQ+PQ")
                      )
  )

  # wmr2025ca ####
  wmr2025ca <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4ca.xlsx"),
    sheet = "ANNEX_Ea",
    range = "A5:T27",
    na = c("", "–"),
    col_names = c(
      "WHO region\nCountry/Area",
      "Survey",
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
      paste("% of children aged <5 years", c(
        "who slept under an ITN",
        "with moderate or severe anaemia",
        "with a positive RDT",
        "with a positive microscopy blood smear"
      )),
      paste("% of children aged <5 years with fever in the past 2 weeks", c(
        "for whom advice or treatment was sought",
        "who had blood taken from a finger or heel for testing",
        "who took antimalarial drugs",
        "who took an ACT among those who received any antimalarial"
      ))
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2025Ca Assertions: ####
  check_who_dataframe(df = wmr2025ca,
                      rows = 22,
                      cols = 21,
                      unique_values = list(
                        `WHO Region` = 1,
                        `Country/area` = 22
                      ),
                      na_values = list(
                        c(2, "% of households with IRS in the past 12 months")
                      ),
                      known_values = list(
                        # The first possible value in the first nontrivial column
                        c(1, "% of households with at least one ITN",  91.5),
                        # The last possible value in the last column
                        c(22, "% of children aged <5 years with fever in the past 2 weeks who took an ACT among those who received any antimalarial", 96.9)
                      )
    )

  # wmr2025cb ####
  # columns from "Fever" onwards have ranges like "20\r\n(18– 21)" but we want them to look like "20 (18, 21)"
  clean_numeric_range <- function(x) {
    # Remove carriage return and replace en dash with comma
    x <- gsub("\r\n", " ", x)
    x <- gsub("–", ",", x)
    # Remove extra spaces
    x <- gsub("\\s+", " ", x)
    # Remove leading and trailing spaces
    x <- trimws(x)
    return(x)
  }
  wmr2025cb <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4cb.xlsx"),
    sheet = "ANNEX_Eb",
    range = "A8:Z32",
    na = c("", "-", "–"),
    # Major names:
    # "Fever prevalence in children aged <5 years", "Health sector where treatment was sought for children aged <5 years", "Diagnostic testing coverage for children aged <5 years in each health sector", "Antimalarial treatment coverage for children aged <5 years in each health sector", "ACT use among antimalarial treatment for children aged <5 years in each health sector"
    # Flat Minor names:
    # Overall	Public excluding community health workers	Community health workers	Formal medical private excluding pharmacies	Pharmacies or accredited drug stores	Informal private	No treatment seeking	Trained provider	Public excluding community health workers	Community health workers	Formal medical private excluding pharmacies	Pharmacies or accredited drug stores	Informal private	Trained provider	Public excluding community health workers	Community health workers	Formal medical private excluding pharmacies	Pharmacies or accredited drug stores	Self-treatment	No treatment seeking	Trained provider	Public	Private	Informal private

    col_names = c(
      "Country/area",
      "Survey",
      "Fever prevalence in children aged <5 years",
      paste("Health sector where treatment was sought for children aged <5 years", c(
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
    dplyr::mutate(`WHO Region`="AFRICAN") |> dplyr::select(`WHO Region`, dplyr::everything()) |>
    dplyr::mutate(dplyr::across(4:27, clean_numeric_range))
  
  ## WMR2025CB Assertions: ####
  
  check_who_dataframe(df = wmr2025cb,
                      rows = 25,
                      cols = 27,
                      unique_values = list(
                        `WHO Region` = 1,
                        `Country/area` = 25
                      ),
                      na_values = list(
                        c(1, "Diagnostic testing coverage in each health sector Community health workers")
                      ),
                      known_values = list(
                        # The first possible value in the first nontrivial column
                        c(1, "Fever prevalence in children aged <5 years", "20 (18, 21)"),
                        # The last possible value in the last column
                        c(24, "ACT use among antimalarial treatment in each health sector Informal private", "97 (78, 100)")
                      )
  )
  
  # wmr2025d ####
  who_regions <- c("AFRICAN", "AMERICAS", "EASTERN MEDITERRANEAN", "SOUTH-EAST ASIA", "WESTERN PACIFIC")
  wmr2025d <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4d.xlsx"),
    sheet = "Sheet1",
    range = "A1:A86",
    na = c("", "–")
  ) |>
    split_who_region(col_region_area = 1) |> # dplyr::distinct(`WHO Region`)
    dplyr::mutate(`WHO Region` = who_regions[as.numeric(forcats::as_factor(`WHO Region`))]) # as_factor keeps the order
  # FOOTNOTES:
  # 1 In the World malaria report 2025, a country or area is considered endemic when it has reported at least one indigenous case since 2022.
  # 2 In May 2013, South Sudan was reassigned to the WHO African Region (resolution WHA66.21, https://apps.who.int/gb/ebwha/pdf_files/WHA66/A66_R21-en.pdf).
  # 3 As of 27 May 2025, Indonesia was reassigned to the WHO Western Pacific Region (resolution WHA78.25, https://apps.who.int/gb/ebwha/pdf_files/WHA78/A78_R25-en.pdf).

  ## WMR2025D Assertions: ####
  check_who_dataframe(df = wmr2025d,
                      rows = 80,
                      cols = 2,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 80
                      ),
                      known_values = list(
                        # First footnote, South Sudan2 at row 37
                        c(37, "Country/area", "South Sudan2"),
                        # The last possible value in the last column
                        c(80, "Country/area", "Viet Nam")
                      )
  )

  ## wmr2025e ####
  wmr2025e <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4e.xlsx"),
    sheet = "Annex L",
    range = "A4:D114",
    na = c("")
  ) |>
    dplyr::rename(`WHO Region` = 1, `Country/area` = 2) |>
    tidyr::fill(`WHO Region`, .direction = "down")

  ## WMR2025E Assertions: ####
  check_who_dataframe(df = wmr2025e,
                      rows = 110,
                      cols = 4,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 110
                      ),
                      na_values = list(
                        c(3, "Year country/territory certified malaria free1,2,3") # Lesotho
                      ),
                      known_values = list(
                        # The last possible value in the last column
                        c(110, "Year country certified malaria free where malaria never existed or disappeared without specific measures4", 2012)
                      )
  )

  # wmr2025f ####
  wmr2025f <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4f.xlsx"),
        sheet = "Annex_F",
        range = "A5:P288",
        na = c("", "–", "-"),
        col_names = c(
          "WHO region\nCountry/Area",
          "Year",
          paste0("Donor_", c("Global Fund", "PMI/USAID", "The World Bank", "UK", "Others")),
          paste0("Country_", c("Government (NMP)", "Government Footnotes", "Global Fund", "PMI/USAID",
                               "The World Bank", "Other bilaterals", "WHO", "UNICEF", "Other contributions7"))
        )
    ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(
      # Translate the footnotes into a boolean column
      `Country_Government Footnotes` = case_match_dict(`Country_Government Footnotes`, dict = c(
        "5" = "WHO NMP funding data estimates.",
        "6" = "Budget not expenditure."
      ))
    )

  ## WMR2025F Assertions: ####
  check_who_dataframe(df = wmr2025f,
                      rows = 279,  # =COUNTA(B6:B288)
                      cols = 17,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 93
                      ),
                      na_values = list(
                        c(1, "Donor_Global Fund")
                      ),
                      known_values = list(
                        # H10, Angola 2023 Government (NMP) funding data estimates
                        c(5, "Country_Government (NMP)", 3498672),
                        # The last possible value in the last column
                        c(279, "Country_Other contributions7", 527091)
                      )
  )

  # wmr2025g ####
  with_fn <- function(colname) {
    c(colname, paste0(colname, "_Footnotes"))
  }
  is_fn <- function(colname) {
    stringr::str_detect(colname, "_Footnotes$")
  }
  # NOTES:
  # ACT: artemisinin-based combination therapy; IRS: indoor residual spraying; ITN: insecticide-treated mosquito net; RDT: rapid diagnostic test; WHO: World Health Organization.
  # “–” refers to data not available.
  # 1 In May 2013, South Sudan was reassigned to the WHO African Region (resolution WHA 66.21, https://apps.who.int/gb/ebwha/pdf_files/WHA66/A66_R21-en.pdf).
  # 2 Where national data for the United Republic of Tanzania are unavailable, refer to Mainland and Zanzibar.
  # 3 As of 27 May 2025, Indonesia was reassigned to the WHO Western Pacific Region (resolution WHA78.25, https://apps.who.int/gb/ebwha/pdf_files/WHA78/A78_R25-en.pdf).
  # FOOTNOTES:
  # $ ACT treatment courses distributed are used to replace missing data for any first-line treatment courses distributed (including ACT).
  # † ACT treatment courses distributed are calculated.
  # ^ The number of malaria cases treated with any first-line treatment courses (including ACT) has been used as a proxy for any first-line treatment courses distributed (including ACT), or the country reports the number of patients treated, rather than the number of treatment courses distributed.
  # # The number of malaria cases treated with ACT has been used as a proxy for ACT treatment courses distributed, or the country reports the number of patients treated rather than the number of treatment courses distributed.
  # § The number of RDTs tested was used as a proxy for the the number of RDTs distributed, where the number of RDTs distributed was missing.
  # ‡ Data reported to the Alliance for Malaria Prevention were used where data reported to WHO were missing or incomplete.
  # * Any first-line treatment courses distributed (including ACT) are calculated.
  # ¶ In the United Republic of Tanzania, IRS was conducted only in Zanzibar.
  data_dict <- c(
    "–" = "Data not available.",
    "$" = "ACT treatment courses distributed are used to replace missing data for any first-line treatment courses distributed (including ACT).",
    "†" = "ACT treatment courses distributed are calculated.",
    "^" = "The number of malaria cases treated with any first-line treatment courses (including ACT) has been used as a proxy for any first-line treatment courses distributed (including ACT), or the country reports the number of patients treated, rather than the number of treatment courses distributed.",
    "#" = "The number of malaria cases treated with ACT has been used as a proxy for ACT treatment courses distributed, or the country reports the number of patients treated rather than the number of treatment courses distributed.",
    "§" = "The number of RDTs tested was used as a proxy for the the number of RDTs distributed, where the number of RDTs distributed was missing.",
    "‡" = "Data reported to the Alliance for Malaria Prevention were used where data reported to WHO were missing or incomplete.",
    "*" = "Any first-line treatment courses distributed (including ACT) are calculated.",
    "¶" = "In the United Republic of Tanzania, IRS was conducted only in Zanzibar."
  )

  wmr2025g <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4g.xlsx"),
    sheet = "ANNEX_G",
    range = "A4:O254",
    na = c("", "–"),
    col_names = c(
      "WHO region\nCountry/Area", "Year",
      "No. of ITNs distributed" |> with_fn(), 
      "Modelled percentage of population with access to an ITN",
      "Number of people protected by IRS" |> with_fn(),
      "Number of RDTs distributed" |> with_fn(),
      "Any first-line treatment courses distributed (including ACT)" |> with_fn(),
      "No. of malaria cases treated with any first-line treatment courses (including ACT)",
      "ACT treatment courses distributed" |> with_fn(),
      "No. of malaria cases treated with ACT"
    )
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(tidyselect::ends_with('_Footnotes'), ~ case_match_dict(.x, data_dict)))

  ## WMR2025G Assertions: ####
  check_who_dataframe(df = wmr2025g,
                      rows = 246, # =COUNTA(B5:B254)
                      cols = 16,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 82
                      ),
                      na_values = list(
                        c(2, "No. of ITNs distributed")
                      ),
                      known_values = list(
                        # The first possible value in the first nontrivial column
                        c(1, "No. of ITNs distributed", 7373106),
                        # The last possible value in the last column
                        c(246, "No. of malaria cases treated with ACT", 201)
                      )
  )

  # wmr2025h ####
  # Read from A5:I2710. 
  # Column names: "WHO region\nCountry/area", "Year", "Population at risk", then:
  # 3 columns for Cases (Lower, Point, Upper) and 3 for Deaths (Lower, Point, Upper)
  # Following precedent set in 2017 data of joining Cases/Deaths with _
  wmr2025h <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4h.xlsx"),
    sheet = "ANNEX_H",
    range = "A5:I2710",
    na = c("", "–"),
    col_names = c(
      "WHO region\nCountry/Area", "Year", "Population at risk",
      paste0("Cases_", c("Lower", "Point", "Upper")),
      paste0("Deaths_", c("Lower", "Point", "Upper"))
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2025H Assertions: ####
  check_who_dataframe(df = wmr2025h,
                      rows = 2700,
                      cols = 10,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 108
                      ),
                      na_values = list(
                        c(1, "Cases_Lower")
                      ),
                      known_values = list(
                        # The first possible value in the first nontrivial column
                        c(1, "Cases_Point", 34),
                        # 2023 values for Angola to compare with whowmr::wmr2024$wmr2024g[1,c(2,4,7,9,14)]
                        c(49, "Population at risk",  36749906), # whole population for Angola
                        c(49, "Cases_Point",  9388568),
                        # The last possible value in the last column
                        c(2693, "Deaths_Upper", 14)
                      )
  )

  # wmr2025i ####
  pcf <- c("Presumed", "Confirmed", "Footnotes")
  footnote_dict <- c(
    "7" = "Figures reported for the public sector include cases detected in the private sector.",
    "8" = "Figures reported for the public sector include cases detected at the community level.",
    "9" = "Figures reported for the public sector include cases detected at the community level and in the private sector.",
    "10" = "Presumed cases are calculated based on the proportion of presumed cases from previous years."
  )

  wmr2025i <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4i.xlsx"),
    sheet = "ANNEX_I",
    range = "A7:N94",
    na = c("", "–"),
    col_names = c(
      "WHO region Country/area",
      paste0("Reported population at risk_", c("Total population", "At risk (high)", "At risk (low)", "Malaria free")),
      paste0("Public sector_", c("Presumed", "Presumed Footnote", "Confirmed", "Confirmed Footnote")),
      paste0("Private sector_", c("Presumed", "Confirmed", "Confirmed Footnote")),
      paste0("Community level_", c("Presumed", "Confirmed"))
    )
  ) |>
    split_who_region(col_region_area = 1) |>
    dplyr::mutate(dplyr::across(tidyselect::contains("Footnote"), ~ case_match_dict(as.character(.x), footnote_dict)))

  ## WMR2025I Assertions: ####
  check_who_dataframe(df = wmr2025i,
                      rows = 83, # =COUNTA(B7:B94)
                      cols = 15,
                      unique_values = list(
                        `WHO Region` = 5,
                        `Country/area` = 83
                      ),
                      na_values = list(
                        c(70, "Public sector_Presumed")
                      ),
                      known_values = list(
                        # The first possible value in the first nontrivial column
                        c(1, "Reported population at risk_Total population", 35121734),
                        # The last possible value in the last column
                        c(83, "Community level_Confirmed", 78)
                      )
  )

  # wmr2025j ####
  # Reported cases by method of confirmation (comparable to 2024 annex h)
  wmr2025j <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4j.xlsx"),
    sheet = "ANNEX_J",
    range = "A4:L916",
    na = c("", "–"),
    col_names = c(
      "WHO region\nCountry/Area",
      "Variable",
      2015:2024
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2025J Assertions: ####
  check_who_dataframe(df = wmr2025j,
                      rows = 907,
                      cols = 13,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 104
                      ),
                      na_values = list(
                        c(7, "2015")
                      ),
                      known_values = list(
                        # The last possible value in the last column
                        c(907, "2024", "100")
                      )
  )

  # wmr2025k ####
  # Malaria cases by species, similar to 2024 annex i
  who_regions <- c("African", "Americas", "Eastern Mediterranean", "European", "South-East Asia", "Western Pacific")
  wmr2025k <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4k.xlsx"),
    sheet = "ANNEX_K",
    range = "A4:L483",
    na = c("", "–"),
    col_names = c(
      "WHO region/Country/area",
      "Species",
      2015:2024
    )
  ) |>
    dplyr::mutate(`WHO region/Country/area` = ifelse(`WHO region/Country/area` %in% who_regions,
      toupper(`WHO region/Country/area`),
      `WHO region/Country/area`
    )) |>
    split_who_region(col_region_area = 1)

  ## WMR2025K Assertions: ####
  check_who_dataframe(df = wmr2025k,
                      rows = 474,
                      cols = 13,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 104
                      ),
                      na_values = list(
                        c(9, "2015")
                      ),
                      known_values = list(
                        # The first possible value in the first nontrivial column (footnotes)
                        c(1, "2015", "0*"),
                        # The last possible value in the last column
                        c(474, "2024", "114")
                      )
  )

  # wmr2025l ####
  # Reported malaria deaths, similar to 2024 annex j
  wmr2025l <- readxl::read_excel(file.path(basePath, "wmr2025_annex_4l.xlsx"),
    sheet = "ANNEX_L",
    range = "A4:K113",
    na = c("", "–"),
    col_names = c(
      "WHO region\nCountry/Area",
      2015:2024
    )
  ) |>
    split_who_region(col_region_area = 1)

  ## WMR2025L Assertions: ####
  check_who_dataframe(df = wmr2025l,
                      rows = 104,
                      cols = 12,
                      unique_values = list(
                        `WHO Region` = 6,
                        `Country/area` = 104
                      ),
                      na_values = list(
                        c(15, "2017"), # D19
                        c(39, "2015")
                      ),
                      known_values = list(
                        # Footnotes K105
                        c(96, "2024", "132*"),
                        # The last possible value in the last column
                        c(104, "2024", "0")
                      )
  )

  list(
    # Annex 2 – Number of ITNs distributed through campaigns in malaria endemic countries, 2022–2024
    wmr2025_2 = wmr2025_2,
    # Annex 4 – Data tables and methods
    # A. Policy adoption, 2024
    wmr2025a = wmr2025a,
    # B. Antimalarial drug policy in malaria endemic countries and areas, 2024
    wmr2025b = wmr2025b,
    # C. Household survey results, 2017–2024
    # a. Compiled through STATcompiler for the WHO African Region
    wmr2025ca = wmr2025ca,
    # b. Compiled through WHO calculations for the WHO African Region
    wmr2025cb = wmr2025cb,
    # D. Malaria endemic countries and areas
    wmr2025d = wmr2025d,
    # E. Countries and areas certified malaria free by WHO (1955–2025) and countries where malaria never existed or disappeared without specific measures
    wmr2025e = wmr2025e,
    # F. Funding for malaria control, 2022–2024
    wmr2025f = wmr2025f,
    # G. Commodities distribution and coverage for malaria endemic countries and areas, 2022–2024
    wmr2025g = wmr2025g,
    # H. Population denominator for case incidence and mortality rate, and estimated malaria cases and deaths, 2000–2024
    wmr2025h = wmr2025h,
    # I. Reported malaria cases by health sector for malaria endemic countries and areas, 2024
    wmr2025i = wmr2025i,
    # J. Reported malaria cases by method of confirmation, 2015–2024
    wmr2025j = wmr2025j,
    # K. Reported malaria cases by species, 2015–2024
    wmr2025k = wmr2025k,
    # L. Reported malaria deaths, 2015–2024
    wmr2025l = wmr2025l
  )
}

wmr2025 <- get_wmr2025()

usethis::use_data(wmr2025, overwrite = TRUE)
