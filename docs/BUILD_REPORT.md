# onet2r 0.4.1 build report

Date: 2026-07-02

## Environment

- Windows 11 x64
- R 4.5.2
- Package version: 0.4.1

## Checks run

- `devtools::document()`: completed after roxygen changes.
- `devtools::build_readme()`: completed after README source changes.
- `devtools::test()`: 342 passing tests.
- Bundled-output smoke checks were rerun against local OEWS, PUMS, archive, measure, aggregation, reconciliation, and change-summary fixtures.
- `urlchecker::url_check()`: all URLs correct.
- `pkgdown::check_pkgdown()`: no problems found.
- `pkgdown::build_site()`: completed successfully.
- All vignettes rendered locally with evaluated code chunks.
- `rcmdcheck::rcmdcheck(args = "--as-cran")`: 0 errors, 0 warnings, 2 notes.

## R CMD check note

1. New submission.
2. Unable to verify current time.

The final R CMD check was run with `ONET_API_KEY` unset so examples could not
make live Web Services calls or expose local credentials. `LC_CTYPE` was also
unset to avoid a local Windows startup warning unrelated to the package. The
future-timestamp note appears to be local to the Windows check environment.

## Optional checks

- `air` was not installed.
- `spelling` was not installed.
- `goodpractice` was not installed.
- `covr` was not installed.

## Output inspection

The rebuilt README and pkgdown articles now use actual `onet2r` function outputs instead of rebuilt example tables except where a user-supplied stylized score is intentionally demonstrated. Tables are rendered with `knitr::kable()`, charts use a shared ggplot2 style with opaque backgrounds, and the historical, OEWS, sensitivity, and decomposition walkthroughs use bundled offline fixtures.
