## Test environments

* local Windows 11 x64, R 4.5.2 (`devtools::test()` and `rcmdcheck::rcmdcheck(args = "--as-cran")`)
* GitHub Actions R-CMD-check matrix, R release on ubuntu-latest, macos-latest, and windows-latest (latest main run 28592118304 succeeded on 2026-07-02)
* win-builder R-devel submitted with `devtools::check_win_devel()` on 2026-07-02; a repeat visible upload returned FTP 550 after the quiet submission completed locally

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  New submission

* checking for future file timestamps ... NOTE
  unable to verify current time

  This appears to be local to the Windows check environment.

## Downstream API behavior

Examples that require live O&#42;NET or Bureau of Labor Statistics web services
are guarded behind interactive checks. The package also includes local sample
Occupational Employment and Wage Statistics data so examples and vignettes can
demonstrate the expected output shape without making network requests.

## Method references

This package is primarily an R client and reproducibility layer for public
O&#42;NET and Bureau of Labor Statistics data products. It does not ship a
substantive exposure score. The vignettes cite the official O&#42;NET Resource
Center documentation and Bureau of Labor Statistics mapping guidance used by the
longitudinal and weighting workflows.

Note: the existing CRAN package ONETr wraps the retired v1 XML API; onet2r
targets the current v2 REST API and the two share no code or namespace.
