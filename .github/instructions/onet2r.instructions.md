# onet2r development instructions

These instructions apply to all package work in this repository.

## Standing rules

- Keep the repository clean. Commit only package source, documentation, tests, fixtures, and site assets that are needed for the package.
- Do not read, print, or commit `.Renviron`, API keys, cached API responses, raw PUMS microdata, or other secrets.
- Do not use em dash characters in code, documentation, vignettes, README, NEWS, commit messages, or generated prose.
- Render O&#42;NET safely in markdown and roxygen prose so it does not become italicized HTML.
- Do not ship, endorse, or assume a substantive exposure measure in exported package functions.
- Stylized measures may appear in tests and articles only when clearly labeled as stylized.
- Public functions use the `onet_` prefix, snake_case arguments, tibble outputs, stable empty schemas, and `cli` errors.
- Prefer native pipes and modern dplyr joins with explicit relationship checks.
- Tests and examples must run without network access and without an O&#42;NET API key unless they are explicitly interactive.
- Verify O&#42;NET, OEWS, and Census facts against primary sources and record durable facts in `docs/DATA_NOTES.md`.
- Update roxygen docs, NEWS, tests, README, vignettes, pkgdown, and `docs/SPEC.md` when public behavior changes.

## Definition of Done

- Changed or new functions have unit tests and have been run on bundled fixture data with output inspected.
- `devtools::document()` has been run after roxygen changes.
- `devtools::test()` has passed.
- `rcmdcheck::rcmdcheck(args = "--as-cran")` has been run before release.
- pkgdown builds locally and generated HTML is checked for broken O&#42;NET rendering.
- Optional release checks are run when the tools are installed: lintr, spelling, urlchecker, goodpractice, and CRAN extra checks.
- Generated local artifacts such as `doc/`, `Meta/`, `*.Rcheck`, package tarballs, and local `_pkgdown/` output are removed before committing unless the repository intentionally tracks them.
