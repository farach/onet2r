## Test environments

* local Windows 11 x64, R 4.5.2

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

## Downstream API behavior

Examples that require live O\*NET or BLS web services are guarded. The package
also includes local sample OEWS data so examples and vignettes can demonstrate
the expected output shape without making network requests.

## Method references

There are no published methodological references for this package. It provides
an R client for public O\*NET and BLS data products.
