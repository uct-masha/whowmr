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
