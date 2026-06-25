## Test environments

* local Windows 11 x64, R 4.5.2

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission

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
