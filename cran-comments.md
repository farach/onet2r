## Submission

This is the first CRAN submission of onet2r.

## Test environments

* Local Windows 11, R 4.6.1 (2026-06-24 ucrt): `R CMD check --as-cran` on the
  built tarball. The local TeX installation has no `makeindex`, so the PDF
  manual was built separately with `R CMD Rd2pdf`.
* GitHub Actions: `R CMD check --as-cran` with R release on ubuntu-latest,
  macos-latest, and windows-latest.
* GitHub Actions: installed-package tests with R 4.1 on ubuntu-22.04, and with
  network access blocked.
* win-builder: R-devel (2026-10-28 r90783 ucrt) and R-release (4.6.2).

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Alex Farach <alexfarach@microsoft.com>'

  New submission

The installed size is about 5.3 Mb. The largest parts are the derived O&#42;NET
data snapshots in `inst/extdata` (2.3 Mb), which let the articles and tests
run offline, and the built vignettes (1.8 Mb).

## Web services and data

Examples that require live O&#42;NET or Bureau of Labor Statistics web services
are guarded behind interactive checks. Tests, examples, and vignettes run
offline from bundled fixtures and data snapshots.

The article data snapshots are derived from the O&#42;NET 30.3 database, which
is licensed under CC BY 4.0, and from Bureau of Labor Statistics data, which
are in the public domain. Each snapshot folder has a README with the
attribution, license, and modification notice. The small archive fixtures in
`inst/extdata/onet-mini` are synthetic.

## Method references

This package is primarily an R client and reproducibility layer for public
O&#42;NET and Bureau of Labor Statistics data products. It does not ship a
substantive exposure score. The vignettes cite the official O&#42;NET Resource
Center documentation and Bureau of Labor Statistics mapping guidance used by the
longitudinal and weighting workflows.

Note: the existing CRAN package ONETr wraps the retired v1 XML API. onet2r
targets the current v2 REST API, and the two share no code or namespace.