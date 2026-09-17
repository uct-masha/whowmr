<!-- https://r-pkgs.org/other-markdown.html#sec-news -->

# whowmr 0.2017.1

-   Initial release with only 2017 data and `getWMRPDFs()` function.

# whowmr 0.2018.1

-   Added 2018 data

# whowmr 1.2018.1

- Changed the format of Annex E (or Ea in future years) which contains a version
  of the DHS/MIS data after retrieving it from the STATCompiler. This is now
  just the table as it appears in the reports for 2017:2018 and will be so in
  future years. Users should get the data from the STATCompiler themselves if
  they want to reproduce the analysis.

- Renamed the getWMRPDFs() function to get_reports() as this is the style used
  in the package.

- Added some new internal functions and made some public ones internal too.

# whowmr 1.2019.1

- Included the 2019 dataset.

# whowmr 1.2020.1

- Included the 2020 dataset.

# whowmr 1.2021.1

- Included the 2021 dataset.

# whowmr 1.2022.1

- Included the 2022 dataset.

# whowmr 1.2023.1

- Included the 2023 dataset.

# whowmr 1.2023.2

- Renamed column `Country` in 2019 annex F to `Country/area` to match the
  other years.

# whowmr 1.2024.2

- Included the 2024 dataset.

# whowmr 1.2025.1

## Breaking changes

- **The annex letters changed in the 2025 report.** WHO relettered the Annex 4
  tables and this package follows the source, so the same letter refers to
  different data in `wmr2025` than it does in `wmr2017` through `wmr2024`. Code
  that builds a sheet name from a year like `wmr[[paste0("wmr", yr, "f")]]` for
  funding will silently return the wrong table for 2025 rather than erroring.

  | Contents                                           | 2017–2024 | 2025 |
  |----------------------------------------------------|-----------|------|
  | Policy adoption                                     | `a`       | `a`  |
  | Antimalarial drug policy                            | `b`       | `b`  |
  | Funding for malaria control                         | `c`       | `f`  |
  | Commodities distribution and coverage               | `d`       | `g`  |
  | Household survey results (STATcompiler)             | `ea`      | `ca` |
  | Household survey results (WHO calculations)         | `eb`      | `cb` |
  | Population denominator, estimated cases and deaths  | `f`       | `h`  |
  | Reported cases by place of care / health sector     | `g`       | `i`  |
  | Reported cases by method of confirmation            | `h`       | `j`  |
  | Reported cases by species                           | `i`       | `k`  |
  | Reported malaria deaths                             | `j`       | `l`  |
  | Malaria endemic countries and areas                 | `k`       | `d`  |
  | Countries certified malaria free                    | `l`       | `e`  |

- Multi-level column headers in the 2025 annexes are joined with an underscore
  (`Public sector_Presumed`) where 2024 mostly used a space
  (`Public sector Presumed`). Note that 2024 was already inconsistent on this
  point. `wmr2024c` used `Donor_Global Fund` whereas 2025 is uniform.

- `wmr2024` now contains 14 sheets rather than 12, so `length(wmr2024)` and
  `names(wmr2024)` return different values than they did in `1.2024.2`.

## New data

- Included the 2025 dataset.
- Included Annexes K and L in the 2024 dataset, as `wmr2024k` (malaria endemic
  countries and areas) and `wmr2024l` (countries and territories certified
  malaria free). These are the same two tables that appear as `wmr2025d` and
  `wmr2025e`.

## Other changes

- Renamed `get_reports()` to `download_whowmr_reports()` to better reflect its
  purpose.
- The vignettes are now built and installed alongside the package. `DESCRIPTION`
  was missing a `VignetteBuilder` field, so earlier releases shipped no
  vignettes at all even though both declared a vignette engine. See
  `browseVignettes("whowmr")`.
- The data processing notebook now documents how the Annex F estimates are
  revised from one report to the next, using Nigeria's 2016 deaths as a worked
  example.
- The package's internal helpers are no longer listed in the help index.
  `help(package = "whowmr")` now shows only `download_whowmr_reports()` and the
  datasets, while `?split_who_region` and the other helpers still work for
  contributors.

## Notes

- In `wmr2024l` and `wmr2025e`, the entries for Azerbaijan and Tajikistan carry
  a trailing non-breaking space in the WHO source. This is preserved, so joining
  on `Country/area` may need `trimws()`.
- **The `1.2024.2` release (tag and GitHub Release) has been removed.** It
  documented the 2024 dataset but was tagged before `data/wmr2024.rda` and its
  generating script were actually committed, so the released package did not
  contain the data it claimed to. This is fixed as of this release.
