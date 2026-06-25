# onet2r 0.4.1 build report

Date: 2026-06-25

## Environment

- Windows 11 x64
- R 4.5.2
- Package version: 0.4.1

## Checks run

- `devtools::document()`: completed after roxygen changes.
- `devtools::build_readme()`: completed after README source changes.
- `devtools::test()`: 297 passing tests with local environment; 294 passing and 2 live smoke tests skipped when `ONET_API_KEY` was unset.
- `Rscript inst/examples/validate-outputs.R`: completed with `ONET_API_KEY` unset, exercising bundled OEWS, archive, measure, aggregation, sensitivity, decomposition, cache, and rate-limit outputs.
- `urlchecker::url_check()`: all URLs correct.
- `lintr::lint_package()`: 203 remaining default line-length findings; no remaining non-line-length lints.
- `pkgdown::check_pkgdown()`: no problems found.
- `pkgdown::build_site(new_process = FALSE)`: completed successfully.
- Generated pkgdown HTML scan: no malformed O&#42;NET rendering, no `# A tibble` console dumps in article output, no base `barplot()` remnants, and no instruction leakage.
- `rcmdcheck::rcmdcheck(args = "--as-cran")`: 0 errors, 0 warnings, 1 note.

## R CMD check note

1. New submission.

The final R CMD check was run with `ONET_API_KEY` unset so examples could not make live Web Services calls or expose local credentials. `LC_CTYPE` was also unset to avoid a local Windows startup warning unrelated to the package.

## Optional checks

- `air` was not installed.
- `spelling` was not installed.
- `goodpractice` was not installed.
- `covr` was not installed.

## Output inspection

The rebuilt README and pkgdown articles now use actual `onet2r` function outputs instead of rebuilt example tables except where a user-supplied stylized score is intentionally demonstrated. Tables are rendered with `knitr::kable()`, charts use a shared ggplot2 style with opaque backgrounds, and the historical, OEWS, sensitivity, and decomposition walkthroughs use bundled offline fixtures.
