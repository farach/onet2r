# onet2r 0.4.0 build report

Date: 2026-06-25

## Environment

- Windows 11 x64
- R 4.5.2
- Package version: 0.4.0

## Checks run

- `devtools::document()`: completed after roxygen changes.
- Exported documentation audit: all exported aliases have `\value{}` and `\examples{}` sections.
- `devtools::test()`: 293 passing tests, 0 failures, 0 warnings, 0 skips.
- `Rscript inst/examples/validate-outputs.R`: completed using bundled fixtures and local example data.
- `urlchecker::url_check()`: all URLs correct.
- `pkgdown::check_pkgdown()`: no problems found.
- `pkgdown::build_site()`: completed successfully.
- Generated pkgdown HTML scan: no malformed O&#42;NET rendering and no instruction leakage.
- `rcmdcheck::rcmdcheck(args = "--as-cran")`: 0 errors, 0 warnings, 2 notes.

## R CMD check notes

1. New submission.
2. Unable to verify current time.

## Optional checks

- `lintr::lint_package()` was run. Non-line-length findings were fixed. The remaining 196 findings are default 80-character line-length warnings in existing source, tests, and vignettes.
- `spelling` was not installed.
- `goodpractice` was not installed.

## Output inspection

The validation script and rebuilt articles exercise the archive, panel reconciliation, measure, aggregation, sensitivity, decomposition, OEWS weight-panel, and PUMS weight-panel workflows on bundled or local example data. Vignettes and README examples render with displayed output and bounded `head()` or summary tables where output would otherwise be too long.
