# whowmr

This package contains datasets from the WHO World Malaria Report:
`whowmr2017`, `whowmr2018`, ..., `whowmr2024`
Each dataset is a list with a tibble for each sheet in the annexes of the 2017 to 2024 World Malaria Reports.

The scripts used to create the package datasets aim to preserve much of the original supporting information such as footnotes and comments.

## Installation

```R

# Install the development version from GitHub
pak::pak("uct-masha/whowmr")
# or
devtools::install_github("uct-masha/whowmr")
```

## Disclaimer

This package is in no way supported or endorsed by the World Health Organization. The data contained in this package is publicly available from the WHO World Malaria Report and links to the original data and reports are given in the help for each dataset.

## Code of Conduct

Please note that the whowmr project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
