# Contributing to whowmr

Contributions to the `whowmr` package are welcome. In particular, issues with the package are very welcome!

Issues, or ideas for features should be reported to the /issues tracker on github.

I am likely to accept sensible pull requests also.

This document is written so that maintainers/contributers can pick up either kind of work described below.

## Versioning and releases

`whowmr` is a slow-cadence package: most years see a single burst of work to add the new dataset (see below), and maybe occasionally a small bug fix in between. Because of that low volume, in 2025 I decided that the rule going forward should be:

**Every commit (or small set of commits) that changes package behaviour is released.** There is no accumulating "unreleased changes" state and no development version suffix. If you fixed a bug, that fix gets a version bump, a `NEWS.md` entry, and a tag - even if it's a single line. Don't batch fixes up "to release later"; later is how fixes silently ship without ever getting a release, which has happened before in this package's history.

The version scheme is `<major>.<year>.<patch>`:

-   **`year`** is the year associated with the most recent World Malaria Report data included, and only moves forward when a new report comes out and its corresponding dataset is added.

-   **`major`** only bumps for a breaking change to an *existing* year's data (such as Annex C's footnote column being reinterpreted for 2019). - not for the arrival of a new year.

-   **`patch`** bumps for anything else: bugfixes, documentation corrections, internal changes, within the current major.year.

A release consists of all of the following, done together, in one small commit:

1.  Bump the `Version:` field in `DESCRIPTION`.

2.  Check/update the most recent year at the top of `README.md`.

3.  Add a `NEWS.md` entry under a new `# whowmr <version>` heading describing what changed.

4.  Commit, push, then tag the commit `vMAJOR.YEAR.PATCH` (matching `DESCRIPTION` exactly) and push the tag.

5.  Cut a GitHub Release from that tag, titled `whowmr MAJOR.YEAR.PATCH`, with the release notes copied from the `NEWS.md` entry.

Keep release titles consistent (`whowmr <version>`, always) - earlier releases in this package's history used inconsistent titles, which makes the Releases page harder to scan.

## Adding a new World Malaria Report dataset

The WHO publishes a new World Malaria Report (and its annexes) once a year. Adding that year's dataset to this package follows the same pattern each time, established in `data-raw/wmr2018.R` onwards. Expect the WHO site's exact layout (annex lettering, URL format, zip vs. individual xlsx links) to shift slightly from year to year - don't assume it will be a pure find-and-replace of the previous year's script. For example 2025 saw a permutation on the annex letters.

1.  **Look at the annex list for the new report** on the WHO site [IRIS Browsing by Title](https://iris.who.int/browse/title?bbm.page=1&startsWith=World%20Malaria%20Report&bbm.sd=DESC). Note the letter/number assigned to each annex and what it covers - this can and does change between years, so don't assume it matches last year's lettering.

2.  **Wire up the download** in `R/ensure_annex_dirs_exist.R`:

    -   Add the new year to the `years` range assertion and the `filenames` vector.

    -   If the WHO page now serves a single zip, the generic download path should just work. If it serves a page of individual `.xlsx` links (as 2024 and 2025 have), you'll need a year-specific download helper or leverage the existing `download_and_zip_2024_and_newer_annexes` if possible.

3.  **Write `data-raw/wmr<year>.R`**, modelled on the most recent year's script. Usually copy from previous year and **carefully** edit. Update all comments at the top. For each annex sheet:

    -   Use `readxl::read_excel()` with an explicit `range` and (where the header row is awkward) explicit `col_names`.

    -   Apply the shared helpers as needed: `split_who_region()`, `case_match_dict()`, `try_make_numeric()`.

    -   **Manually check the parsed data against the source spreadsheet.** This is the step that catches misaligned ranges, shifted columns, unexpected footnote characters, and NA sentinels that differ from previous years. See "Manually checking a sheet" below.

    -   Once you're confident the sheet is correct, write a `check_who_dataframe(...)` assertion block for it (see `R/check_who_dataframe.R`) recording the row/column count, a couple of unique-value counts, at least one known NA cell, and at least one known cell value. This assertion is what protects the dataset against silent regressions if the script is ever re-run against updated source files.

    -   Assemble all the sheets into a named list and call `usethis::use_data(wmr<year>, overwrite = TRUE)` at the bottom of the script, then actually run the script so `data/wmr<year>.rda` is produced.

4.  **Manually checking a sheet.** The convenience of accessing World Malaria Report data in R is only as valuable as the integrity of the process mapping it into R. The data must be faithful to the original World Malaria Report source. There's no shortcut for this - you need to look at the actual spreadsheet and compare it to what was parsed. Some things that help:

    -   Open the excel sheet as well as the previous years version (which there is code for) to understand the differences.

    -   Watch for non-numeric characters that will silently fail to parse (stray minus-sign look-alikes, footnote markers, thousands separators, carriage returns).

    -   Write and run each sheet's assertion for each sheet yourself and run it before moving on. Make sure you manually check against the excel sheet that what ends up in the data object is correct, so that writing it forces you to actually look at the values you're asserting on. Keep footnotes in the data, as is the case with all other code, as this pushes users towards engaging with them (since they provide important context).

5.  **Document the dataset** in `R/data.R`: copy the most recent year's roxygen block, relabel it, and update the page numbers, sheet descriptions, and `@source` URLs (both the report PDF and the annexes page - check these actually resolve, WHO has changed their PDF hosting format before). Some annexes are not in the report, so find the list of them on the WHO publication page and document them as I did in the 2025 dataset.

6.  **Update `vignettes/whowmr.Rmd`** to reference the new year. This is the main vignette that demonstrates how to use the package and should include a brief example of accessing the new dataset.

7.  **Update `README.md`** examples to reference the new year.

8.  **Note anything surprising** you found while checking the data in `vignettes/data-processing-notebook.Rmd` - inconsistent terminology, footnote quirks, figures that changed between report vintages, etc. This document exists specifically to save the next person (including future you) from rediscovering the same gotcha.

9.  **Regenerate package documentation**: `devtools::document(roclets = c('rd', 'collate', 'namespace'))`. This creates `man/wmr<year>.Rd` and keeps `NAMESPACE` in sync.

10. **Release**, following the "Versioning and releases" process above. This normally means bumping to `<major>.<new year>.1` unless the new year also happens to bundle a breaking change to an existing year's data.