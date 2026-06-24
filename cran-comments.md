## Test environments

* local Windows 11 x64, R 4.5.2

## R CMD check results

There are no package-owned test failures or notes after running the unit test
suite and vignette builds locally.

The local `devtools::check()` run is currently blocked during DESCRIPTION
metadata checking by the installed Quarto command-line wrapper on this Windows
machine. The same failure reproduces outside package checking with:

```r
system2("quarto", "-V", env = paste0("TMPDIR=", tempdir()))
```

Quarto interprets the `TMPDIR=...` environment assignment as a command rather
than an environment variable. Package tests and vignette builds pass outside
that environment-specific Quarto probe.

## Downstream API behavior

Examples that require live O*NET or BLS web services are guarded. The package
also includes local sample OEWS data so examples and vignettes can demonstrate
the expected output shape without making network requests.

## Method references

There are no published methodological references for this package. It provides
an R client for public O*NET and BLS data products.
