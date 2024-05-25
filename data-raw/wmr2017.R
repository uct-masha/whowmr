# The downloadData.R file exposes `ensureRawDataExists` which fetches raw data
# from the WHO website and unzips it to a local directory.
# This file brings the unzipped data from the 2017 World Malaria Report into R
# and saves it as a dataset in this package.

# Annex overview from WHO World Malaria Report 2017:
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

  wmr2017a <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-a.xls"), range = "A2:Q102") |>
    split_who_region(col_region_area=1, col_na_when_region=NA)

  wmr2017b <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-b.xls"), range = "A2:F102") |>
    split_who_region(col_region_area=1, col_na_when_region=NA)

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
    split_who_region(col_region_area=1, col_na_when_region=NA)
  # Translate the footnotes into a boolean column
  wmr2017c[,"Budget not expenditure"] <- !is.na(wmr2017c[,"Budget not expenditure"])

  wmr2017d <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-d.xls"), range = "A1:I294") |>
    split_who_region()

  wmr2017e <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-e.xlsx"),
                                 sheet="DATA", range="A6:Q106",
                                 col_names = c("WHO region/Country/area", "Source",
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
  # Note: Might want to bring in Region because this just has Country/area now

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
    split_who_region()
  wmr2017f <- wmr2017f[,!grepl(x=colnames(wmr2017f),"^Blank")]

  wmr2017fa <- wmr2017f  # confirmed with all.equal
  # Note: fb has 2 sheets. The first is a pivot of the second. We will read the
  # second since it is more detailed.
  wmr2017fb <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-f-b.xlsx"),
                                  range="A2:D1752", sheet=2,
                                  col_names <- c(
                                    "WHO Region",
                                    "Country/area",
                                    "Year",
                                    "Population at Risk"
                                  ))
  # Following the convention that WHO REGION is uppercase
  wmr2017fb$`WHO Region` <- toupper(wmr2017fb$`WHO Region`)

  wmr2017g <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-g.xls"),
                                 range="A4:K100",
                                 col_names = c(
                                   "WHO region Country/area",
                                   "UN population",
                                   "At risk (low + high)",
                                   "At risk (high)",
                                   "Number of people living in active foci",
                                   paste(c(
                                     "Public sector",
                                     "Private sector",
                                     "Community level"),
                                     rep(c("Presumed", "Confirmed"), 3))
                                 )) |>
    split_who_region()
  # NOTE: Tanzania is split into Mainland and Zanzibar
  #       When making the summary table do not double count Tanzania
  # Also the summary table A104:K109 differs in Pub/Com Presumed cases for the Americas
  # One would expect 0 from the data but gets 20057 and	2 respectively in the regional summary
  # I confirmed all other values are the same using
  # =SUM(B5:B50)-B46 ; =SUM(B52:B70) ; =SUM(B72:B79) ; =SUM(B81:B89) ; =SUM(B91:B100)
  # Conditional format applies to =$L$105:$U$109 ; Cell value not equals `=B105`

  wmr2017h <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-h.xls"),
                                 range="A2:I631",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Variable",
                                   2010:2016
                                 )) |>
    split_who_region()
  # NOTE: Tanzania, Mainland and Zanzibar are split again
  # I did not check the Regional Summary

  wmr2017i <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-i.xls"),
                                 range="A2:I423",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   "Species",
                                   2010:2016
                                 )) |>
    split_who_region()

  wmr2017j <- readxl::read_excel(file.path(basePath,"wmr2017-annex-table-j.xls"),
                                 range="A2:H111",
                                 col_names = c(
                                   "WHO region/Country/area",
                                   2010:2016
                                 )) |>
    split_who_region()
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
