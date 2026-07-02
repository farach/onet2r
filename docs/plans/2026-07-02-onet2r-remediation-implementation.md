# onet2r Remediation & Completion — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. If you have neither skill, execute tasks strictly in order, one at a time, running every verification step exactly as written.

**Goal:** Fix one blocking regression, close all remaining audit findings, and add four research-driven features so onet2r (v0.4.1) is a defensible CRAN submission.

**Architecture:** onet2r is an httr2-based client for the O*NET Web Services API 2.0 plus three research modules: OEWS wage integration (flat-file download/parse), PUMS employment weighting (aggregation only — fetching is delegated to tidycensus), and longitudinal O*NET archive panels (download historical releases, reconcile changes, decompose). All outputs are tibbles; all caching lives under `tools::R_user_dir("onet2r", "cache")`.

**Tech Stack:** R (>= 4.1), httr2 (>= 1.0.0), dplyr (>= 1.1.0), purrr, tibble, rlang, cli, lifecycle; testthat (>= 3.0.0) edition 3; roxygen2 with markdown.

**Companion spec:** `docs/plans/2026-07-02-onet2r-remediation-design.md` (approved). Read it first.

## Global Constraints

- **CRAN policy:** no unconditional network access in tests, examples, or vignettes. Tests mock network bindings via `testthat::local_mocked_bindings()`. Examples that hit the network use `@examplesIf interactive() && nzchar(Sys.getenv("ONET_API_KEY"))` or `if (interactive())`. Vignettes run only on bundled fixtures.
- **Caching:** only under `tools::R_user_dir("onet2r", "cache")`, in named sections (`api`, `archives`, `crosswalks`, `oews`); every new section must be clearable via `onet_cache_clear()`.
- **Style:** native pipe `|>` only (never `%>%`), `dplyr::join_by()` for joins with explicit `relationship =`, `.data`/`.env` pronouns in data-masking, cli-styled errors (`cli::cli_abort()` with `{.arg}`/`{.val}` markup), snake_case names, all exported functions prefixed `onet_`, tibble outputs.
- **Line references** in this plan were verified against main @ 2783494 on 2026-07-02. If the file has drifted, locate the code by the quoted context, not the line number.
- **Windows verification quirk:** on this machine, `Rscript -e '...' | tail` frequently swallows ALL output. Never pipe `Rscript -e`. Write a script file and run `Rscript path/to/script.R` directly.
- **Package loading in ad-hoc scripts:** `suppressPackageStartupMessages(pkgload::load_all(".", export_all = TRUE, quiet = TRUE))` from the package root gives access to internal functions.
- **Roxygen:** after changing any roxygen comment run `Rscript -e 'devtools::document()'` and commit the regenerated `man/` and `NAMESPACE` files with the same commit.
- **Every task ends with the full test suite green:** `Rscript -e 'devtools::test()'` → `[ FAIL 0 | WARN 0 | ... ]`. Do not proceed to the next task otherwise.
- **Commits:** one commit per task minimum, message style matches repo history (imperative, no scope prefix needed).

---

### Task 1 (P0): Fix the `download_crosswalk_file()` regression

**Why:** Commit 5277a15 pasted `download_crosswalk_file()` *inside* `read_adjacent_crosswalk()` (R/panel.R:880-921) *after* its call site (R/panel.R:869). Any real (uncached, unmocked) `onet_crosswalk_bridge()` call fails with `could not find function "download_crosswalk_file"`. The 342-test suite passes because both bridge tests mock `read_adjacent_crosswalk` entirely — mocks sit above the broken layer. Live-confirmed on 2026-07-02.

**Files:**
- Modify: `R/panel.R:864-921` (hoist nested function)
- Test: `tests/testthat/test-panel.R`

- [ ] **Step 1: Write the failing regression tests**

Append to `tests/testthat/test-panel.R`:

```r
test_that("download_crosswalk_file is a package-level function", {
  expect_true(is.function(onet2r:::download_crosswalk_file))
})

test_that("read_adjacent_crosswalk parses a cached crosswalk CSV end to end", {
  cache_dir <- withr::local_tempdir()
  local_mocked_bindings(onet_cache_dir = function() cache_dir)
  crosswalk_dir <- file.path(cache_dir, "crosswalks")
  dir.create(crosswalk_dir, recursive = TRUE)

  csv <- paste(
    '"O*NET-SOC 2010 Code","O*NET-SOC 2010 Title","O*NET-SOC 2019 Code","O*NET-SOC 2019 Title"',
    '"15-1132.00","Software Developers, Applications","15-1252.00","Software Developers"',
    '"15-1133.00","Software Developers, Systems Software","15-1252.00","Software Developers"',
    '"29-1141.00","Registered Nurses","29-1141.00","Registered Nurses"',
    sep = "\n"
  )
  # File name must match what download_crosswalk_file() computes as dest
  writeLines(csv, file.path(crosswalk_dir, "2010_to_2019_Crosswalk.csv"))

  out <- onet2r:::read_adjacent_crosswalk("2010", "2019")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3L)
  expect_setequal(
    as.character(out$map_type[out$from_onet_soc_code == "29-1141.00"]),
    "one_to_one"
  )
  # 15-1252.00 receives two sources -> merge classification on the target side
  expect_true(all(
    as.character(out$map_type[out$to_onet_soc_code == "15-1252.00"]) == "merge"
  ))
})
```

Note: `adjacent_crosswalk_url("2010", "2019")` ends in `2010_to_2019_Crosswalk.csv?fmt=csv`, so `basename(url)` includes the `?fmt=csv` query string. Step 3 strips query strings when computing the cache filename (`sub("\\?.*$", "", basename(url))`); the fixture filename above (`2010_to_2019_Crosswalk.csv`) matches that stripped name. If you change the stripping logic, keep the fixture filename in sync.

- [ ] **Step 2: Run the new tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "panel")'`
Expected: FAIL — `could not find function "download_crosswalk_file"` (or object not found for the first test).

- [ ] **Step 3: Hoist the function**

In `R/panel.R`, cut the entire nested definition (lines 880-921, from `download_crosswalk_file <- function(url, cache_dir = onet_cache_dir()) {` through its closing `}`) out of `read_adjacent_crosswalk()` and paste it at top level **above** `read_adjacent_crosswalk` (i.e., immediately after `vintage_path()` ends at line 862). While moving it, apply the query-string fix so cache filenames are clean:

```r
download_crosswalk_file <- function(url, cache_dir = onet_cache_dir()) {
  crosswalk_dir <- file.path(cache_dir, "crosswalks")
  dir.create(crosswalk_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(crosswalk_dir, sub("\\?.*$", "", basename(url)))
  if (file.exists(dest) && file.info(dest)$size > 0) {
    return(dest)
  }
  tmp <- tempfile("onet-crosswalk-", tmpdir = crosswalk_dir, fileext = ".csv")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  old_options <- options(
    HTTPUserAgent = "onet2r (https://github.com/farach/onet2r)",
    timeout = max(300, getOption("timeout"))
  )
  on.exit(options(old_options), add = TRUE)
  status <- tryCatch(
    utils::download.file(url = url, destfile = tmp, mode = "wb", quiet = TRUE),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to download O*NET crosswalk.",
          "i" = "URL: {.url {url}}"
        ),
        parent = cnd
      )
    }
  )
  if (!identical(status, 0L)) {
    cli::cli_abort(
      c(
        "Failed to download O*NET crosswalk.",
        "i" = "URL: {.url {url}}"
      )
    )
  }
  if (file.info(tmp)$size <= 0) {
    cli::cli_abort("Downloaded O*NET crosswalk was empty.")
  }
  if (!file.rename(tmp, dest)) {
    cli::cli_abort("Failed to move downloaded O*NET crosswalk into the cache.")
  }
  dest
}
```

`read_adjacent_crosswalk()` keeps its existing body minus the nested definition — the call `path <- download_crosswalk_file(url)` now resolves to the package-level function.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "panel")'`
Expected: PASS, 0 failures.

- [ ] **Step 5: Live smoke test (requires network)**

Write `verify_bridge.R` in a temp directory:

```r
suppressPackageStartupMessages(pkgload::load_all(".", export_all = FALSE, quiet = TRUE))
onet_cache_clear(what = "crosswalks")
r <- onet_crosswalk_bridge("2010", "2019")
cat("bridge rows:", nrow(r), "\n")
stopifnot(nrow(r) > 900, all(c("map_type", "crosswalk_weight") %in% names(r)))
cat("OK\n")
```

Run: `Rscript verify_bridge.R` (from the package root).
Expected: `bridge rows: <n>` (roughly 1,000-1,200) then `OK`. If offline, defer this step to Task 16's final gate — do not skip it permanently.

- [ ] **Step 6: Commit**

```bash
git add R/panel.R tests/testthat/test-panel.R
git commit -m "Fix download_crosswalk_file regression: hoist to package level"
```

---

### Task 2: `onet_occupation_details()` returns a tibble, not an unnamed list

**Why:** Live-verified: the function returns an unnamed 17-element list. Each element is `list(href = <url>, title = <section name>)` (structure confirmed against the live API on 2026-07-02). An unnamed list is unusable and violates the package's tibble-output convention.

**Files:**
- Modify: `R/occupations.R:110-131`
- Test: create `tests/testthat/test-occupation-details.R`

**Interfaces:**
- Produces: `onet_occupation_details(code)` → tibble with columns `title` (chr), `href` (chr). Stable empty schema (0 rows, same columns) when the section list is absent.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-occupation-details.R`:

```r
test_that("onet_occupation_details returns a tibble of sections", {
  local_mocked_bindings(
    onet_occupation = function(code) {
      list(
        code = code,
        title = "Software Developers",
        details_contents = list(
          list(href = "https://api-v2.onetcenter.org/online/occupations/15-1252.00/details/tasks",
               title = "Tasks"),
          list(href = "https://api-v2.onetcenter.org/online/occupations/15-1252.00/details/skills",
               title = "Skills")
        )
      )
    }
  )
  out <- onet_occupation_details("15-1252.00")
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("title", "href"))
  expect_equal(out$title, c("Tasks", "Skills"))
})

test_that("onet_occupation_details returns a stable empty schema", {
  local_mocked_bindings(
    onet_occupation = function(code) list(code = code, title = "X")
  )
  out <- onet_occupation_details("15-1252.00")
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("title", "href"))
  expect_equal(nrow(out), 0L)
})
```

- [ ] **Step 2: Run to verify failure**

Run: `Rscript -e 'devtools::test(filter = "occupation-details")'`
Expected: FAIL — current function returns a bare list.

- [ ] **Step 3: Implement**

Replace the body of `onet_occupation_details()` in `R/occupations.R` (currently lines 122-131) and update its roxygen `@return` to: "A tibble with columns `title` and `href`, one row per available details section. Zero rows when the occupation has no details sections." Match the file's existing roxygen conventions (e.g., how `O*NET` is escaped in titles).

```r
onet_occupation_details <- function(code) {
  validate_onet_code(code)

  details <- onet_occupation(code)$details_contents
  schema <- tibble::tibble(title = character(), href = character())
  if (is.null(details) || length(details) == 0) {
    return(schema)
  }

  purrr::map(details, \(x) {
    tibble::tibble(
      title = as.character(x$title %||% NA_character_),
      href = as.character(x$href %||% NA_character_)
    )
  }) |>
    purrr::list_rbind()
}
```

- [ ] **Step 4: Document and run tests**

Run: `Rscript -e 'devtools::document(); devtools::test()'`
Expected: PASS across the whole suite (nothing else calls this function internally, but confirm).

- [ ] **Step 5: Commit**

```bash
git add R/occupations.R man/onet_occupation_details.Rd tests/testthat/test-occupation-details.R
git commit -m "Return a tibble of sections from onet_occupation_details"
```

---
### Task 3: Account for bridge-unmapped occupations in reconciliation

**Why:** `onet_panel_reconcile()` routes everything through `inner_join(from_panel, pair_bridge)` (R/panel.R:1146-1154). A from-release occupation with no bridge row appears in neither matched rows nor coverage rows — it silently vanishes, biasing `onet_change_summary()` shares. Vignettes teach hand-built (incomplete) bridges, so this is the norm, not an edge case. Symmetrically, a to-release code absent from the bridge is currently conflated with genuinely-new occupations.

**Files:**
- Modify: `R/panel.R` — `change_type_levels()` (~line 1113), `coverage_status_levels()` (~line 1125), `reconcile_coverage_rows()` (~lines 1140-1193), `new_coverage_rows()` (locate with grep)
- Test: `tests/testthat/test-panel.R`

**Interfaces:**
- Produces: two new factor levels, `"unmapped_source"` and `"unmapped_target"`, in BOTH `change_type` and `coverage_status`. Rows with these statuses always have `safely_comparable = FALSE`. Task 4 (change summary) consumes these levels automatically; the NEWS entry in Task 16 must mention them.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-panel.R`:

```r
test_that("reconcile reports occupations missing from the bridge", {
  panel <- tibble::tibble(
    release_version = c("20.1", "20.1", "25.1", "25.1"),
    release_date = as.Date(c("2015-10-01", "2015-10-01", "2020-11-01", "2020-11-01")),
    soc_vintage = factor(c("2010", "2010", "2019", "2019"),
      levels = onet2r:::onet_vintage_levels
    ),
    domain = "Abilities",
    onet_soc_code = c("11-1011.00", "13-1199.00", "11-1011.00", "13-1198.00"),
    soc_code = c("11-1011", "13-1199", "11-1011", "13-1198"),
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = factor("IM"),
    data_value = c(4.5, 3.9, 4.6, 4.0),
    source_date = as.Date(c("2014-07-01", "2014-07-01", "2019-07-01", "2019-07-01")),
    domain_source = factor("Analyst")
  )
  # Bridge maps only 11-1011.00; 13-1199.00 (source) and 13-1198.00 (target)
  # are absent from the bridge.
  bridge <- tibble::tibble(
    from_vintage = "2010", to_vintage = "2019",
    from_onet_soc_code = "11-1011.00", to_onet_soc_code = "11-1011.00",
    map_type = "one_to_one", crosswalk_weight = 1
  )

  out <- onet_panel_reconcile(panel, bridge)

  unmapped_src <- out[out$coverage_status == "unmapped_source", ]
  expect_equal(unmapped_src$from_onet_soc_code, "13-1199.00")
  expect_true(all(!unmapped_src$safely_comparable))

  unmapped_tgt <- out[out$coverage_status == "unmapped_target", ]
  expect_equal(unmapped_tgt$to_onet_soc_code, "13-1198.00")
  expect_true(all(!unmapped_tgt$safely_comparable))
  expect_equal(nrow(out[out$coverage_status == "matched", ]), 1L)
})
```

- [ ] **Step 2: Run to verify failure**

Run: `Rscript -e 'devtools::test(filter = "panel")'`
Expected: FAIL — no `unmapped_source` level exists.

- [ ] **Step 3: Implement**

3a. Extend the level definitions in `R/panel.R`, appending the two new levels at the end (existing order matters to factor-comparing tests). Verify the existing `change_type_levels()` body at HEAD first and only append — do not reorder:

```r
change_type_levels <- function() {
  c(
    "stale_carryforward",
    "real_update",
    "resampled_stable",
    "recode_or_recalc_flag",
    "source_date_missing",
    "transition_data",
    "suppressed_change",
    "new",
    "dropped",
    "unmapped_source",
    "unmapped_target"
  )
}

coverage_status_levels <- function() {
  c("matched", "new", "dropped", "unmapped_source", "unmapped_target")
}
```

3b. In `reconcile_coverage_rows()`, after the `mapped_from` join, compute unmapped source rows:

```r
  unmapped_source <- from_panel |>
    dplyr::anti_join(
      pair_bridge,
      by = dplyr::join_by(
        onet_soc_code == from_onet_soc_code,
        soc_vintage == from_vintage
      )
    )
```

and change the return to:

```r
  dplyr::bind_rows(
    dropped_coverage_rows(dropped, from_release, to_release, to_panel),
    new_coverage_rows(new, from_release, to_release, from_panel, pair_bridge),
    unmapped_source_rows(unmapped_source, from_release, to_release, to_panel)
  )
```

3c. Add the builder next to `dropped_coverage_rows()`. **Critical:** its column list must exactly match `empty_reconciled_panel()` at HEAD — copy `dropped_coverage_rows()` and change only the fields shown below; if `empty_reconciled_panel()` has columns beyond these, mirror them with the same NA-typed defaults `dropped_coverage_rows()` uses. A mismatch makes `bind_rows()` widen frames silently.

```r
unmapped_source_rows <- function(unmapped, from_release, to_release, to_panel) {
  if (nrow(unmapped) == 0) {
    return(empty_reconciled_panel())
  }

  tibble::tibble(
    from_release = from_release,
    to_release = to_release,
    from_release_date = unmapped$release_date,
    to_release_date = single_date(to_panel$release_date),
    from_onet_soc_code = unmapped$onet_soc_code,
    to_onet_soc_code = NA_character_,
    from_soc_code = standardize_soc_code(unmapped$onet_soc_code),
    to_soc_code = NA_character_,
    soc_vintage_from = unmapped$soc_vintage,
    soc_vintage_to = single_vintage(to_panel$soc_vintage),
    domain = unmapped$domain,
    element_id = unmapped$element_id,
    element_name = unmapped$element_name,
    scale_id = unmapped$scale_id,
    from_value = unmapped$data_value,
    to_value = NA_real_,
    value_change = NA_real_,
    value_percent_change = NA_real_,
    from_source_date = unmapped$source_date,
    to_source_date = as.Date(NA),
    from_domain_source = as.character(unmapped$domain_source),
    to_domain_source = NA_character_,
    from_recommend_suppress = as.character(unmapped$recommend_suppress),
    to_recommend_suppress = NA_character_,
    from_not_relevant = as.character(col_or_na(unmapped, "not_relevant", nrow(unmapped))),
    to_not_relevant = NA_character_,
    date_changed = NA,
    value_changed = NA,
    transition_data = is_transition_source(unmapped$domain_source),
    suppressed_change = is_suppressed_estimate(unmapped$recommend_suppress),
    change_type = factor("unmapped_source", levels = change_type_levels()),
    coverage_status = factor("unmapped_source", levels = coverage_status_levels()),
    method_break = FALSE,
    crosswalk_uncertain = TRUE,
    safely_comparable = FALSE,
    map_type = factor(NA_character_, levels = map_type_levels()),
    crosswalk_weight = NA_real_
  )
}
```

3d. In `new_coverage_rows()`, add a `pair_bridge` parameter (signature becomes `function(new, from_release, to_release, from_panel, pair_bridge)`) and split genuinely-new from bridge-gap targets. Before the `tibble()` call compute:

```r
  status <- dplyr::if_else(
    new$onet_soc_code %in% pair_bridge$to_onet_soc_code,
    "new",
    "unmapped_target"
  )
```

then inside the tibble replace the hardcoded factors:

```r
    change_type = factor(status, levels = change_type_levels()),
    coverage_status = factor(status, levels = coverage_status_levels()),
```

and change `map_type` to:

```r
    map_type = factor(
      dplyr::if_else(status == "new", "new", NA_character_),
      levels = map_type_levels()
    ),
```

- [ ] **Step 4: Run tests**

Run: `Rscript -e 'devtools::test(filter = "panel")'` then the full `devtools::test()`.
Expected: all PASS. If existing reconcile tests assert exact factor level sets, update them to include the two new levels — that is an intended contract change.

- [ ] **Step 5: Document**

In the roxygen for `onet_panel_reconcile()` `@return`, append: `Rows with coverage_status "unmapped_source"/"unmapped_target" mark occupations absent from the supplied bridge; they are never safely_comparable. An incomplete hand-built bridge produces many of these — inspect them before interpreting change shares.` Run `devtools::document()`.

- [ ] **Step 6: Commit**

```bash
git add R/panel.R man/ tests/testthat/test-panel.R
git commit -m "Track bridge-unmapped occupations in reconcile coverage"
```

---

### Task 4: Crosswalk-weighted distribution in `onet_change_summary()`

**Why:** Reconciled split/merge rows appear once per branch; `onet_change_summary()` (R/panel.R:426-500) counts them unweighted and ignores the `crosswalk_weight` column. A 1-to-3 split inflates its family's row count 3x.

**Files:**
- Modify: `R/panel.R:426-500`
- Test: `tests/testthat/test-panel.R`

**Interfaces:**
- Produces: two new columns on `onet_change_summary()` output: `n_weighted` (dbl) and `share_weighted` (dbl). Existing columns unchanged.

- [ ] **Step 1: Write the failing test**

```r
test_that("change summary weights split rows by crosswalk_weight", {
  reconciled <- tibble::tibble(
    to_soc_code = c("11-1011", "11-1011", "15-1252"),
    change_type = factor(c("real_update", "real_update", "stale_carryforward")),
    safely_comparable = TRUE,
    crosswalk_weight = c(0.5, 0.5, 1)
  )
  out <- onet_change_summary(reconciled)
  real <- out[out$change_type == "real_update", ]
  expect_equal(real$n, 2L)             # unweighted count unchanged
  expect_equal(real$n_weighted, 1.0)   # two half-weight branches = one occupation
  expect_equal(real$share_weighted, 0.5)
})
```

- [ ] **Step 2: Verify failure** — `Rscript -e 'devtools::test(filter = "panel")'` → FAIL (`n_weighted` not found).

- [ ] **Step 3: Implement**

In `onet_change_summary()` after the existing column back-fills (lines 432-443) add:

```r
  if (!"crosswalk_weight" %in% names(data)) {
    data$crosswalk_weight <- 1
  }
  data$crosswalk_weight[is.na(data$crosswalk_weight)] <- 1
```

In `summarize_change_group()` replace the distribution block (lines 492-499) with:

```r
  total_weight <- sum(data$crosswalk_weight)
  distribution <- data |>
    dplyr::summarise(
      n = dplyr::n(),
      n_weighted = sum(.data$crosswalk_weight),
      .by = "change_type"
    ) |>
    dplyr::mutate(
      change_type = as.character(.data$change_type),
      share = .data$n / n_pairs,
      share_weighted = .data$n_weighted / total_weight
    ) |>
    dplyr::arrange(.data$change_type)
```

Note: `summarize_change_group()` receives `data` as a parameter, so the weight back-fill in `onet_change_summary()` guarantees `crosswalk_weight` exists here.

Extend the `n_pairs == 0` early-return tibble (line 489) to:

```r
      tibble::tibble(
        change_type = NA_character_, n = 0L, n_weighted = 0,
        share = NA_real_, share_weighted = NA_real_
      )
```

Update the `@return` roxygen with: `n_weighted and share_weighted down-weight split/merge branch rows by their crosswalk_weight so a 1-to-3 split counts as one occupation, not three.`

- [ ] **Step 4: Run tests** — `Rscript -e 'devtools::document(); devtools::test()'` → PASS.

- [ ] **Step 5: Commit**

```bash
git add R/panel.R man/onet_change_summary.Rd tests/testthat/test-panel.R
git commit -m "Add crosswalk-weighted counts to change summary"
```

---

### Task 5: Refuse NA release dates in reconcile ordering

**Why:** `onet_panel_reconcile()` orders releases by `release_date` (R/panel.R:385-387). Local archives read without a `release_date` produce `as.Date(NA)`, which sorts last silently — adjacent-pair comparisons can be wrong with no signal.

**Files:**
- Modify: `R/panel.R:385-391`
- Test: `tests/testthat/test-panel.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("reconcile aborts on missing release dates", {
  panel <- tibble::tibble(
    release_version = c("29.0", "30.0"),
    release_date = as.Date(c("2024-08-01", NA)),
    soc_vintage = factor("2019", levels = onet2r:::onet_vintage_levels),
    domain = "Abilities",
    onet_soc_code = "11-1011.00",
    soc_code = "11-1011",
    element_id = "1.A.1.a.1",
    scale_id = factor("IM"),
    data_value = c(4.5, 4.6),
    source_date = as.Date("2023-08-01"),
    domain_source = factor("Analyst")
  )
  bridge <- tibble::tibble(
    from_vintage = "2019", to_vintage = "2019",
    from_soc_code = "11-1011", to_soc_code = "11-1011",
    map_type = "one_to_one", crosswalk_weight = 1
  )
  expect_error(onet_panel_reconcile(panel, bridge), "release_date")
})
```

- [ ] **Step 2: Verify failure** — currently returns silently-ordered output, so `expect_error` FAILs.

- [ ] **Step 3: Implement**

In `onet_panel_reconcile()` immediately after the `releases <- ...` pipeline insert:

```r
  if (anyNA(releases$release_date)) {
    missing_versions <- releases$release_version[is.na(releases$release_date)]
    cli::cli_abort(c(
      "Every release in {.arg panel} needs a non-missing {.var release_date} to order comparisons.",
      "x" = "Missing for version{?s}: {.val {missing_versions}}.",
      "i" = "For local archives, pass {.arg release_dates} to {.fun onet_panel} or {.fun onet_archive_read}."
    ))
  }
```

- [ ] **Step 4: Run tests** — `Rscript -e 'devtools::test()'` → PASS (if any existing test fed NA dates, give it real dates).

- [ ] **Step 5: Commit**

```bash
git add R/panel.R tests/testthat/test-panel.R
git commit -m "Abort reconcile when release dates are missing"
```

---
### Task 6: OEWS ANNUAL/HOURLY flags + wage-field documentation

**Why:** OEWS files carry `annual`/`hourly` flag columns (annual-only occupations like teachers have no `h_*` values; hourly-only like actors have no `a_*`). The cleaner leaves them as raw character and nothing documents that `h_median` is structurally NA for some occupations — users misread structural NAs as suppression.

**Files:**
- Modify: `R/oews.R` — the cleaning function (locate with `grep -n "clean_oews_data" R/oews.R`; it is the function that snake_cases names and applies `parse_oews_number()` to `oews_numeric_columns()`) and `onet_oews()` roxygen
- Modify: `inst/extdata/oews-national-sample.csv`
- Test: `tests/testthat/test-oews.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("annual/hourly flags are coerced to logical", {
  raw <- tibble::tibble(
    OCC_CODE = c("25-2021", "27-2011"),
    O_GROUP = "detailed",
    TOT_EMP = c("1000", "500"),
    A_MEDIAN = c("65000", "*"),
    H_MEDIAN = c("*", "25.10"),
    ANNUAL = c("TRUE", ""),
    HOURLY = c("", "TRUE")
  )
  out <- onet2r:::clean_oews_data(raw)
  expect_type(out$annual, "logical")
  expect_type(out$hourly, "logical")
  expect_equal(out$annual, c(TRUE, FALSE))
  expect_equal(out$hourly, c(FALSE, TRUE))
})
```

(Adjust the helper name if it differs at HEAD — follow the call chain from `onet_oews()`.)

- [ ] **Step 2: Verify failure** — flags come back character.

- [ ] **Step 3: Implement**

In the cleaning function, after numeric coercion of `oews_numeric_columns()`, add:

```r
  flag_cols <- intersect(names(out), c("annual", "hourly"))
  for (col in flag_cols) {
    out[[col]] <- toupper(trimws(as.character(out[[col]]))) %in% c("TRUE", "T", "YES", "Y")
  }
```

In `onet_oews()` roxygen add a `@details` paragraph (verbatim; if the topcode/suppression indicator columns added in 5277a15 use different names — check `grep -n "topcoded" R/oews.R` — cite the actual names):

```
#' @details
#' ## Wage-field semantics
#' OEWS publishes both hourly (`h_*`) and annual (`a_*`) wage fields. Some
#' occupations are annual-only (`annual = TRUE`; e.g., teachers) or
#' hourly-only (`hourly = TRUE`; e.g., actors) - their missing counterpart
#' fields are structural, not suppressed. Suppression and top-coding are
#' flagged separately (see the `*_topcoded` and suppression indicator
#' columns). `*_prse` columns are percent relative standard errors of the
#' corresponding estimates.
```

Add `ANNUAL`/`HOURLY` columns to `inst/extdata/oews-national-sample.csv` (blank for most rows, `TRUE` where realistic) so the vignette exercises the coercion.

- [ ] **Step 4: Run tests** — `Rscript -e 'devtools::document(); devtools::test()'` → PASS.

- [ ] **Step 5: Commit**

```bash
git add R/oews.R man/onet_oews.Rd inst/extdata/oews-national-sample.csv tests/testthat/test-oews.R
git commit -m "Coerce OEWS annual/hourly flags and document wage-field semantics"
```

---

### Task 7: Document the OEWS release-timing assumption

**Why:** `latest_oews_year()` (R/oews.R:268-274) assumes the May-Y estimates are out once the calendar reaches April Y+1. BLS has slipped past April before; when that happens the default-year download 404s with no hint why.

**Files:**
- Modify: `R/oews.R:268-289` (docs + error hint only — no behavior change)

- [ ] **Step 1: Improve the validation error**

In `validate_oews_year()` replace the abort message (lines 283-285) with:

```r
    cli::cli_abort(c(
      "{.arg year} must be a supported May OEWS estimate year, 2003 or later.",
      "i" = "The default assumes BLS publishes May estimates by early April of the following year; if the newest release is not out yet, pass an earlier {.arg year} explicitly."
    ))
```

- [ ] **Step 2: Document in `onet_oews()` roxygen**, appending to the `@param year` text: `Defaults to the most recent May estimates assumed available (previous calendar year from April onward, two years back before April). If BLS has not yet published that release, the download fails - pass the prior year explicitly.`

- [ ] **Step 3: Run** `Rscript -e 'devtools::document(); devtools::test()'` → PASS.

- [ ] **Step 4: Commit**

```bash
git add R/oews.R man/
git commit -m "Document OEWS default-year timing assumption"
```

---

### Task 8: Remove dead code

**Why:** `extract_paged_data()` (R/request.R:284-290) and `onet_first_list_key()` (R/occupations.R:749-758) are defined but never called (verified by grep on 2026-07-02).

**Files:**
- Modify: `R/request.R`, `R/occupations.R`

- [ ] **Step 1: Confirm still unreferenced**

Run: `grep -rn "extract_paged_data\|onet_first_list_key" R/ tests/ vignettes/`
Expected: only the two definitions. If a caller has appeared, STOP and leave that function alone.

- [ ] **Step 2: Delete both functions** including their roxygen comment blocks.

- [ ] **Step 3: Run** `Rscript -e 'devtools::document(); devtools::test()'` → PASS; no new check NOTEs.

- [ ] **Step 4: Commit**

```bash
git add R/request.R R/occupations.R man/
git commit -m "Remove unused pagination helpers"
```

---

### Task 9: Guard live smoke tests for CRAN and offline machines

**Why:** `tests/testthat/test-smoke.R` skips only when `ONET_API_KEY` is unset. On CRAN that is a de facto skip, but a keyed developer machine without network fails spuriously; `skip_on_cran()` is standard belt-and-braces.

**Files:**
- Modify: `tests/testthat/test-smoke.R`, `DESCRIPTION`

- [ ] **Step 1: Add guards**

At the top of every `test_that()` block in `test-smoke.R` that performs a live call, make the first three lines:

```r
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
```

(keeping the existing `skip_if_not(nzchar(Sys.getenv("ONET_API_KEY")))` after them).

- [ ] **Step 2: Add `curl` to `Suggests`** in DESCRIPTION (alphabetical position).

- [ ] **Step 3: Run** `Rscript -e 'devtools::test(filter = "smoke")'` → PASS or SKIP (both acceptable).

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-smoke.R DESCRIPTION
git commit -m "Guard live smoke tests with skip_on_cran and skip_if_offline"
```

---

### Task 10: `onet_releases()` — memoise, format column, floor documentation, all-NA guard

**Why:** (a) `onet_archive_read()` without `path` scrapes the releases page twice per call (once inside `onet_archive_download()` at R/panel.R:89, once in `resolve_archive_release_date()`). (b) Callers cannot tell text archives from legacy ZIPs. (c) The auto-download floor and legacy-format caveats are undocumented. (d) **Silent all-NA panels:** `onet_standardize_archive_table()` maps columns via `col_or_na()`; if an older archive uses different headers, every value comes back NA with no error — the worst failure mode for a research package.

**Files:**
- Modify: `R/panel.R` — `onet_releases()` (lines 22-53), `onet_standardize_archive_table()` (locate with grep)
- Test: `tests/testthat/test-panel.R`

**Interfaces:**
- Produces: `onet_releases(refresh = FALSE)` gains a `format` column (`"text"` or `"legacy_zip"`) and per-session memoisation via internal `clear_release_cache()`. `onet_standardize_archive_table()` aborts when the SOC-code column is entirely absent.

- [ ] **Step 1: Write the failing tests**

```r
test_that("onet_releases memoises the release page per session", {
  calls <- 0L
  local_mocked_bindings(
    onet_read_lines = function(url) {
      calls <<- calls + 1L
      '<a href="/dl_files/database/db_30_3_text.zip">x</a>'
    }
  )
  onet2r:::clear_release_cache()
  r1 <- onet_releases()
  r2 <- onet_releases()
  expect_equal(calls, 1L)
  expect_equal(r1$format, "text")
  r3 <- onet_releases(refresh = TRUE)
  expect_equal(calls, 2L)
})

test_that("archive tables with no SOC column abort loudly", {
  bad <- data.frame(`Wrong Header` = c("a", "b"), check.names = FALSE)
  expect_error(
    onet2r:::onet_standardize_archive_table(bad, "9.0", "Abilities", as.Date("2006-01-01")),
    "O\\*NET-SOC Code"
  )
})
```

Also add `onet2r:::clear_release_cache()` at the top of any EXISTING test that mocks `onet_read_lines` for `onet_releases()` — the new memoisation would otherwise serve a cached copy and break those tests.

- [ ] **Step 2: Verify failure** — no `refresh` argument, no `format` column, no guard.

- [ ] **Step 3: Implement**

3a. At the top of `R/panel.R` (after the constants block, lines 5-7):

```r
.onet2r_release_cache <- new.env(parent = emptyenv())

clear_release_cache <- function() {
  rm(list = ls(.onet2r_release_cache), envir = .onet2r_release_cache)
}
```

3b. Rework `onet_releases()`. This is the existing body at HEAD with exactly three changes — memoisation check at top, `format =` field, cache-store before return. Diff against HEAD before committing:

```r
onet_releases <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && !is.null(.onet2r_release_cache$releases)) {
    return(.onet2r_release_cache$releases)
  }
  html <- paste(onet_read_lines(onet_release_archive_url), collapse = "\n")
  links <- onet_extract_links(html)

  text_links <- links[
    grepl("/dl_files/database/db_[0-9_]+_text\\.zip$", links) |
      grepl("/dl_files/db_[0-9_]+\\.zip$", links)
  ]
  versions <- purrr::map_chr(text_links, archive_version_from_link)

  headings <- onet_release_headings(html)

  out <- purrr::map(seq_along(versions), \(i) {
    version <- versions[[i]]
    heading <- headings[headings$version == version, , drop = FALSE]
    release_date <- if (nrow(heading) > 0) heading$release_date[[1]] else NA
    month <- if (nrow(heading) > 0) heading$month[[1]] else NA_character_
    year <- if (nrow(heading) > 0) heading$year[[1]] else NA_character_
    tibble::tibble(
      version = version,
      release_date = as.Date(release_date, origin = "1970-01-01"),
      year = parse_onet_integer(year),
      month = month,
      soc_vintage = onet_soc_vintage(version),
      format = if (grepl("_text\\.zip$", text_links[[i]])) "text" else "legacy_zip",
      text_url = onet_absolute_url(text_links[[i]]),
      dictionary_url = onet_dictionary_url(version, links)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::distinct(.data$version, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$release_date), dplyr::desc(.data$version))

  .onet2r_release_cache$releases <- out
  out
}
```

3c. Roxygen for `onet_releases()`:

```
#' @param refresh Set `TRUE` to re-scrape the releases page instead of using
#'   the per-session cache.
#' @details
#' Text archives (`format = "text"`) exist for releases 20.1 (October 2015)
#' onward. Earlier releases (5.0-20.0) are published as legacy ZIPs
#' (`format = "legacy_zip"`); onet2r can download them, but their internal
#' file layouts vary and parsing is verified only for 20.1+. The O*NET
#' production archive begins at 5.0 (April 2003), the first release of the
#' Data Collection Program - no survey-based data exists before that.
```

3d. All-NA guard — in `onet_standardize_archive_table()`, immediately after `onet_soc_code <- as.character(col_or_na(data, "O*NET-SOC Code", n_rows))`:

```r
  if (n_rows > 0 && all(is.na(onet_soc_code))) {
    cli::cli_abort(c(
      "Archive table {.val {table}} (version {.val {version}}) has no {.val O*NET-SOC Code} column.",
      "i" = "Columns found: {.val {names(data)}}.",
      "i" = "This release's file layout is not supported for panel assembly."
    ))
  }
```

- [ ] **Step 4: Run tests** — `Rscript -e 'devtools::document(); devtools::test()'` → PASS. (`resolve_archive_release_date()` and `onet_archive_download()` now hit the memoised copy automatically; the memoisation test proves one scrape per session.)

- [ ] **Step 5: Commit**

```bash
git add R/panel.R man/onet_releases.Rd tests/testthat/test-panel.R
git commit -m "Memoise releases, add format column and all-NA archive guard"
```

---

### Task 11: Fixture realism + parse-layer fixture tests

**Why:** (a) The `tiny_archive_zip()` helper leaves the process cwd changed until teardown (test-panel.R:30-31). (b) onet-mini fixtures include a `Title` column real Abilities files do not have, and never exercise spaced file names (`Task Ratings.txt`) — the path real users hit. (c) API-endpoint tests mock above the JSON-parse layer, so schema drift is invisible.

**Files:**
- Modify: `tests/testthat/test-panel.R` (helper), `inst/extdata/onet-mini/db_30_3_text/Abilities.txt`, `DESCRIPTION`
- Create: `tests/testthat/fixtures/skills_body.json`, `tests/testthat/test-parse-layer.R`

- [ ] **Step 1: Fix the helper's cwd handling**

In `tiny_archive_zip()` replace:

```r
  old_dir <- setwd(tmp)
  withr::defer(setwd(old_dir), testthat::teardown_env())
  zipfile <- file.path(tmp, "db_30_3_text.zip")
  utils::zip(
    zipfile,
    files = file.path("db_30_3_text", "Abilities.txt"),
    flags = "-q"
  )
```

with:

```r
  zipfile <- file.path(tmp, "db_30_3_text.zip")
  withr::with_dir(tmp, {
    utils::zip(
      "db_30_3_text.zip",
      files = c(
        file.path("db_30_3_text", "Abilities.txt"),
        file.path("db_30_3_text", "Task Ratings.txt")
      ),
      flags = "-q"
    )
  })
```

- [ ] **Step 2: Add a spaced-name member and a test**

In the helper, after the existing `write.table()` for `Abilities.txt`, add:

```r
  write.table(
    tibble::tibble(
      `O*NET-SOC Code` = c("15-1252.00", "29-1141.00"),
      `Task ID` = c("20461", "12013"),
      `Scale ID` = c("IM", "IM"),
      `Data Value` = c(4.2, 4.6),
      `N` = c(30L, 28L),
      Date = c("07/2025", "08/2025"),
      `Domain Source` = c("Incumbent", "Incumbent")
    ),
    file = file.path(source_dir, "Task Ratings.txt"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
```

then add the test:

```r
test_that("archive reader resolves spaced table names", {
  zip <- tiny_archive_zip()
  out <- onet_archive_read("30.3", "Task Ratings", path = zip,
                           release_date = as.Date("2025-08-01"))
  expect_s3_class(out, "tbl_df")
  expect_true(all(!is.na(out$onet_soc_code)))
})
```

- [ ] **Step 3: Remove the `Title` column from `inst/extdata/onet-mini/db_30_3_text/Abilities.txt`** (real Abilities files carry no Title; titles live in Occupation Data.txt). Re-run `devtools::test()`; update any test or vignette that asserted a populated `title` column from this fixture (expect `NA` now — that matches real data).

- [ ] **Step 4: Parse-layer fixture test**

Create `tests/testthat/fixtures/skills_body.json` (shape verified against the live v2 API on 2026-07-02 — output columns `id, related, name, description, importance`):

```json
{
  "start": 1, "end": 2, "total": 2,
  "element": [
    {"id": "2.A.1.a", "related": "https://api-v2.onetcenter.org/x",
     "name": "Reading Comprehension",
     "description": "Understanding written sentences.",
     "importance": 75},
    {"id": "2.A.1.b", "related": "https://api-v2.onetcenter.org/y",
     "name": "Active Listening",
     "description": "Giving full attention.",
     "importance": 72}
  ]
}
```

Create `tests/testthat/test-parse-layer.R`:

```r
test_that("skills parse a realistic response body end to end", {
  body <- jsonlite::fromJSON(
    test_path("fixtures", "skills_body.json"),
    simplifyVector = FALSE
  )
  local_mocked_bindings(onet_perform = function(req) body)
  out <- onet_skills("15-1252.00")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("id", "related", "name", "description", "importance"))
  expect_equal(out$importance, c(75, 72))
})
```

Add `jsonlite` to `Suggests` in DESCRIPTION (used directly in tests; it is already a transitive dependency of httr2). If a maintainer with a live key can re-record this fixture from a real response, prefer the recording; otherwise the JSON above is faithful to the live schema.

- [ ] **Step 5: Run** `Rscript -e 'devtools::test()'` → PASS.

- [ ] **Step 6: Commit**

```bash
git add tests/testthat/ inst/extdata/onet-mini/ DESCRIPTION
git commit -m "Harden fixtures: cwd-safe zips, spaced names, parse-layer test"
```

---

### Task 12: Documentation batch

**Why:** Six small documentation/validation gaps, individually verified OPEN or PARTIAL on 2026-07-02. No behavior changes except one added validation.

**Files:**
- Modify: `R/census.R`, `R/weights.R`, `R/decomposition.R`, `R/request.R`, `R/crosswalks.R`

- [ ] **Step 1: Scope the CPS claim in `R/census.R`**

The deprecated `onet_pums_employment_weights()` roxygen (lines 5-10) says "American Community Survey (ACS), Current Population Survey (CPS), or similar microdata". Replace that opening description with:

```
#' Converts American Community Survey (ACS) PUMS microdata with occupation
#' codes into SOC-level employment weights. (CPS microdata uses different
#' weights and reference periods; see [onet_weight_panel_pums()] for the
#' replicate-weight-aware successor and its CPS ASEC notes.)
#' Use this after downloading PUMS data with packages such as `tidycensus` or
#' `ipumsr`.
```

- [ ] **Step 2: 5-year PUMS caveat in `R/weights.R`**

In `onet_weight_panel_pums()` roxygen `@details`, append:

```
#' The `year` argument means the 1-year ACS PUMS data year. 5-year PUMS files
#' pool responses across vintages and can mix SOCP code versions within one
#' file; SOC-vintage labeling is not reliable for them. Prefer 1-year files
#' for weight panels, or resolve codes explicitly with a crosswalk.
```

- [ ] **Step 3: Stale-carryforward semantics in `R/decomposition.R`**

In `onet_decompose_change()` roxygen `@details`, append:

```
#' Rows classified `stale_carryforward` by [onet_panel_reconcile()] have
#' identical values in both releases by construction, so they contribute
#' exactly zero within-occupation change. If your question is about *true*
#' measured change, restrict `comparable` to `real_update` and
#' `resampled_stable` rows before decomposing; including carry-forwards
#' shrinks the within component toward zero mechanically.
```

- [ ] **Step 4: Cache-expiry note in `R/request.R`**

In `onet_cache_use()` roxygen `@details`, append:

```
#' Cached responses are keyed by URL only and never expire automatically.
#' O*NET publishes new database releases roughly quarterly; run
#' [onet_cache_clear()] after a release, or whenever you need current data.
```

- [ ] **Step 5: Military-crosswalk validation + docs in `R/crosswalks.R`**

`onet_crosswalk_military()` forwards `start`/`end` unvalidated and returns 0 rows for some valid MOC codes (live-verified for `"11B"` on 2026-07-02). Add the same range validation the occupation endpoints use (locate the helper with `grep -n "validate_range\|validate_pagination" R/occupations.R` and call it identically), and append to the roxygen:

```
#' @details
#' `keyword` is matched by the O*NET API against military occupation codes
#' and titles. Some valid MOC codes return zero rows when the service cannot
#' infer the branch; searching by title keyword (e.g., "infantry") is more
#' reliable than bare codes.
```

- [ ] **Step 6: Run** `Rscript -e 'devtools::document(); devtools::test()'` → PASS.

- [ ] **Step 7: Commit**

```bash
git add R/ man/
git commit -m "Documentation batch: CPS scope, 5-year PUMS, stale semantics, cache expiry, military crosswalk"
```

---
### Task 13 (feature): `onet_data_updates()` — official longitudinal update record

**Why:** O*NET publishes a *Longitudinal Data Updates* spreadsheet recording which occupations were re-rated in each release (linked from <https://www.onetcenter.org/dataUpdates.html>; existence and purpose verified 3-0 in adversarial review). This is ground truth for the rolling-update problem — the package currently only *infers* re-rating from value/date changes. Note: O*NET restates historical update counts against the current O*NET-SOC 2019 taxonomy (923 data-collection occupations), so cross-year counts need taxonomy context — say so in the docs.

**Files:**
- Create: `R/data_updates.R`, `tests/testthat/test-data-updates.R`
- Modify: `R/request.R` (`onet_cache_clear` sections), `DESCRIPTION` (verify readxl is in Suggests), `_pkgdown.yml` (reference index)

**Interfaces:**
- Produces: `onet_data_updates(path = NULL, force = FALSE)` → tibble with (at minimum) `onet_soc_code` (chr), `title` (chr), plus one column per update cycle as published. Internal `parse_data_updates(data)` does the normalization and is unit-testable without readxl or network.

- [ ] **Step 1: Verify the download URL (network required)**

Fetch <https://www.onetcenter.org/dataUpdates.html> and locate the XLSX link whose text mentions "Longitudinal". Record the absolute URL as the constant `onet_data_updates_url` in `R/data_updates.R`. **Do not guess this URL** — read it from the page. Also open the spreadsheet once and note the actual sheet name and column headers; adjust the `parse_data_updates()` column mapping in Step 3 to match what you observe (prefer an exact `dplyr::rename()` on the observed names over the grep fallback shown).

- [ ] **Step 2: Write the failing test (parser only — no network)**

```r
test_that("parse_data_updates normalizes the update sheet", {
  raw <- tibble::tibble(
    `O*NET-SOC Code` = c("11-1011.00", "15-1252.00"),
    `Title` = c("Chief Executives", "Software Developers"),
    `Cycle 24 (2023)` = c("", "X"),
    `Cycle 25 (2024)` = c("X", "")
  )
  out <- onet2r:::parse_data_updates(raw)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("onet_soc_code", "title", "cycle_24_2023", "cycle_25_2024"))
  expect_equal(out$onet_soc_code, c("11-1011.00", "15-1252.00"))
})
```

- [ ] **Step 3: Implement `R/data_updates.R`**

```r
# Verified from https://www.onetcenter.org/dataUpdates.html on <DATE OF STEP 1>.
onet_data_updates_url <- "<URL RECORDED IN STEP 1>"

#' Get the O*NET Longitudinal Data Updates Record
#'
#' Downloads and parses the official O*NET Longitudinal Data Updates
#' spreadsheet, which records which occupations were re-rated in each data
#' collection cycle. Use it as ground truth alongside the change
#' classification from [onet_panel_reconcile()]: a `real_update` row for an
#' occupation the official record says was not re-rated deserves scrutiny,
#' and vice versa.
#'
#' @param path Optional path to a local copy of the spreadsheet. If supplied,
#'   no download is attempted.
#' @param force Re-download even when a cached copy exists.
#' @return A tibble with `onet_soc_code`, `title`, and one column per update
#'   cycle as published by O*NET.
#' @details
#' O*NET restates historical update counts against the current O*NET-SOC
#' 2019 taxonomy (923 data-collection occupations), so per-cycle counts are
#' not comparable across taxonomy versions without bridging - see
#' [onet_crosswalk_bridge()].
#'
#' The file is cached under `tools::R_user_dir("onet2r", "cache")` in the
#' `reference` section; clear it with `onet_cache_clear(what = "reference")`.
#' @export
#' @examples
#' if (interactive()) {
#'   updates <- onet_data_updates()
#'   head(updates)
#' }
onet_data_updates <- function(path = NULL, force = FALSE) {
  rlang::check_installed("readxl", reason = "to read the data updates spreadsheet.")
  file <- path %||% download_data_updates_file(force = force)
  raw <- readxl::read_excel(file)
  parse_data_updates(raw)
}

download_data_updates_file <- function(cache_dir = onet_cache_dir(), force = FALSE) {
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(reference_dir, sub("\\?.*$", "", basename(onet_data_updates_url)))
  if (file.exists(dest) && file.info(dest)$size > 0 && !isTRUE(force)) {
    return(dest)
  }
  tmp <- tempfile("onet-updates-", tmpdir = reference_dir, fileext = ".xlsx")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  old_options <- options(
    HTTPUserAgent = "onet2r (https://github.com/farach/onet2r)",
    timeout = max(300, getOption("timeout"))
  )
  on.exit(options(old_options), add = TRUE)
  status <- tryCatch(
    utils::download.file(onet_data_updates_url, tmp, mode = "wb", quiet = TRUE),
    error = function(cnd) {
      cli::cli_abort(
        c("Failed to download the O*NET data updates spreadsheet.",
          "i" = "URL: {.url {onet_data_updates_url}}",
          "i" = "Download it manually and pass {.arg path}."),
        parent = cnd
      )
    }
  )
  if (!identical(status, 0L) || file.info(tmp)$size <= 0) {
    cli::cli_abort("Failed to download the O*NET data updates spreadsheet.")
  }
  if (!file.rename(tmp, dest)) {
    cli::cli_abort("Failed to move the data updates spreadsheet into the cache.")
  }
  dest
}

parse_data_updates <- function(data) {
  data <- tibble::as_tibble(data)
  names(data) <- to_snake_case(gsub("[^A-Za-z0-9]+", "_", names(data)))
  code_col <- grep("soc", names(data), value = TRUE)[[1]]
  title_col <- grep("title", names(data), value = TRUE)[[1]]
  data |>
    dplyr::rename(
      onet_soc_code = dplyr::all_of(code_col),
      title = dplyr::all_of(title_col)
    ) |>
    dplyr::mutate(onet_soc_code = standardize_onet_soc_code(.data$onet_soc_code))
}
```

(`to_snake_case()` and `standardize_onet_soc_code()` already exist as package internals — grep for their definitions and confirm the names before use. The `<URL RECORDED IN STEP 1>` placeholder is intentional and MUST be replaced with the URL you verified in Step 1; do not commit the placeholder.)

- [ ] **Step 4: Extend `onet_cache_clear()`** (R/request.R:175-178): add `"reference"` to the `what` choices vector and to the `"all"` expansion.

- [ ] **Step 5: Run tests** — `Rscript -e 'devtools::document(); devtools::test()'` → PASS. Then one manual network verification: a script calling `onet_data_updates()` and printing `dim()` + `names()`; confirm columns match the roxygen and adjust `parse_data_updates()` if the real sheet differs from the Step 2 fixture.

- [ ] **Step 6: Add `- onet_data_updates` to `_pkgdown.yml`** under the longitudinal reference section.

- [ ] **Step 7: Commit**

```bash
git add R/data_updates.R R/request.R tests/testthat/test-data-updates.R man/ NAMESPACE _pkgdown.yml
git commit -m "Add onet_data_updates(): official longitudinal update record"
```

---

### Task 14 (feature): OEWS unmatched-code diagnostics in aggregation

**Why:** BLS does not publish every detailed 2018 SOC in OEWS — some occupations appear only as OEWS-specific combinations (source: bls.gov/oes/soc_2018.htm; this claim was NOT adversarially verified — **verify in Step 1 before implementing**). Weight-panel SOCs that can never match an O*NET measure silently sit in the denominator.

**Files:**
- Modify: `R/measure.R` (`onet_measure_aggregate()` — locate the coverage computation with `grep -n "covered_employment" R/measure.R`)
- Test: `tests/testthat/test-measure.R`

- [ ] **Step 1: Verify the premise (network required)**

Read <https://www.bls.gov/oes/soc_2018.htm> and the latest OEWS field descriptions. Confirm that some 2018 SOC detailed occupations are published only at broad/OEWS-combination level. If TRUE → proceed as written. If FALSE → still implement the diagnostic (unmatched employment also arises from military/residual SOCs) but reword the message's final line to drop the OEWS-combination claim.

- [ ] **Step 2: Write the failing test**

```r
test_that("aggregate reports the largest unmatched weight-panel SOCs", {
  measure <- onet_measure(
    tibble::tibble(onet_soc_code = "11-1011.00", score = 0.5),
    key = "onet_soc_code", score = "score"
  )
  weights <- tibble::tibble(
    reference_soc_code = c("11-1011", "55-1011"),
    employment = c(100, 900),
    weight_share = c(0.1, 0.9),
    source = "test", source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC", year = 2024
  )
  expect_message(
    onet_measure_aggregate(measure, weights),
    "55-1011"
  )
})
```

(Match the weight-panel column requirements enforced by `filter_aggregate_weight_panel()` — copy the minimal valid panel shape from the existing aggregate tests in `test-measure.R` and adapt. Match `onet_measure()`'s real signature from its roxygen — the call above follows the documented `data, key, score` pattern; correct it if HEAD differs.)

- [ ] **Step 3: Implement**

In `onet_measure_aggregate()`, after coverage is computed, add (adapting the local variable names — the validated weight panel and the matched/mapped scores table — to the actual names in the function body):

```r
  unmatched <- weight_panel |>
    dplyr::anti_join(
      dplyr::distinct(mapped, .data$reference_soc_code),
      by = "reference_soc_code"
    )
  if (nrow(unmatched) > 0) {
    unmatched_share <- sum(unmatched$employment, na.rm = TRUE) /
      sum(weight_panel$employment, na.rm = TRUE)
    if (unmatched_share > 0.05) {
      top <- unmatched |>
        dplyr::arrange(dplyr::desc(.data$employment)) |>
        utils::head(5)
      cli::cli_inform(c(
        "{round(100 * unmatched_share, 1)}% of weight-panel employment has no matching measure score.",
        "i" = "Largest unmatched SOC{?s}: {.val {top$reference_soc_code}}.",
        "i" = "OEWS omits some detailed SOCs (military, residual, and OEWS-specific combinations); unmatched employment stays in the denominator of {.field employment_coverage_share}."
      ))
    }
  }
```

- [ ] **Step 4: Run** `Rscript -e 'devtools::test()'` → PASS. If existing aggregate tests now emit the message under `expect_silent`, wrap their calls in `suppressMessages()` without weakening substantive assertions.

- [ ] **Step 5: Commit**

```bash
git add R/measure.R tests/testthat/test-measure.R
git commit -m "Report unmatched weight-panel employment in aggregates"
```

---

### Task 15: Vignette enrichment, ecosystem positioning, agent instructions

**Why:** Three research-verified facts belong in the docs; CRAN reviewers will ask how onet2r relates to ONETr; the `.github/instructions` file is what future coding agents read and it lacks the module map.

**Files:**
- Modify: `vignettes/longitudinal-onet-background.Rmd`, `vignettes/measure-reproducibility.Rmd`, `README.Rmd`, `cran-comments.md`, `inst/REFERENCES.bib`, `.github/instructions/onet2r.instructions.md`

- [ ] **Step 1: Add references to `inst/REFERENCES.bib`**

```bibtex
@article{handel2016onet,
  author  = {Handel, Michael J.},
  title   = {The {O*NET} content model: strengths and limitations},
  journal = {Journal for Labour Market Research},
  year    = {2016},
  volume  = {49},
  number  = {2},
  pages   = {157--176},
  doi     = {10.1007/s12651-016-0199-8}
}

@techreport{onet_aoskills_update,
  author      = {{National Center for O*NET Development}},
  title       = {Analyst Occupational Skills Ratings: Procedures Update},
  institution = {National Center for O*NET Development},
  url         = {https://www.onetcenter.org/reports/AOSkills_ProcUpdate.html},
  year        = {2011}
}
```

(Verify the AOSkills report year from the page and correct if needed.)

- [ ] **Step 2: `vignettes/longitudinal-onet-background.Rmd`** — add a subsection after the existing Analyst/Incumbent discussion (locate the heading with grep; insert verbatim):

```markdown
### Re-ratings are not independent

Two facts sharpen the carry-forward caution. First, the Skills domain moved
from incumbent to trained-analyst ratings around 2008, a documented
comparability break (Handel 2016). Second — and less well known — for
occupations rated a second time, O*NET's analyst procedures explicitly
provide prior ratings to the analysts (National Center for O*NET
Development, Analyst Occupational Skills Ratings: Procedures Update). Modern
release-over-release "changes" in analyst-rated domains are therefore not
independent re-measurements; treat small movements as measurement
persistence, not signal. The official
[Longitudinal Data Updates record](https://www.onetcenter.org/dataUpdates.html)
(see `onet_data_updates()`) tells you which occupations were actually
re-rated in each cycle.
```

- [ ] **Step 3: `vignettes/measure-reproducibility.Rmd`** — add one paragraph where IM/LV scale choice is first discussed:

```markdown
A practical note on choosing between the Importance (1–5) and Level (0–7)
scales: they are empirically near-redundant. Handel (2016) reports a mean
within-descriptor correlation of 0.92 across 130,249 ratings, with roughly
19% of correlations at 0.98 or above. A measure built from IM and one built
from LV will usually rank occupations almost identically — so treat an
IM-vs-LV sensitivity check as cheap insurance, not as two independent
measures.
```

- [ ] **Step 4: `README.Rmd` positioning + cran-comments**

Add before the license/footer section of `README.Rmd`:

```markdown
## Related tools

- **ONETr** (CRAN) wraps the retired O*NET Web Services v1 XML API with
  username/password auth; it predates the v2 API this package targets.
- **blscrapeR / blsAPI** wrap the BLS *time-series* API, which does not
  serve the OEWS research flat files onet2r ingests — complementary, not
  overlapping.
- **tidycensus / ipumsr** are the right tools for *fetching* ACS PUMS
  microdata; onet2r deliberately has no Census fetcher and consumes their
  output for employment weights.
- No other R or Python tool assembles longitudinal panels from historical
  O*NET database releases; that machinery is unique to onet2r.
```

Re-knit: `Rscript -e 'devtools::build_readme()'`.

Add one sentence to `cran-comments.md`: `Note: the existing CRAN package ONETr wraps the retired v1 XML API; onet2r targets the current v2 REST API and the two share no code or namespace.`

- [ ] **Step 5: `.github/instructions/onet2r.instructions.md`** — append:

```markdown
## Module map

- R/auth.R, R/request.R — API key, request construction, retry, rate limit,
  opt-in response cache (tools::R_user_dir sections: api, archives,
  crosswalks, oews, reference; onet_cache_clear(what=) clears each).
- R/search.R, R/occupations.R, R/database.R, R/crosswalks.R — API endpoints.
  All endpoints return tibbles with stable empty schemas; _all variants
  auto-paginate with a non-advancing-cursor guard.
- R/oews.R — BLS OEWS flat-file download/parse. o_group filtering, topcode
  and suppression flags, atomic cached downloads.
- R/census.R (deprecated), R/weights.R, R/weighted.R — employment weight
  panels from user-supplied PUMS/OEWS data. No Census fetcher by design.
- R/panel.R — longitudinal archives: releases scrape (memoised), archive
  download/read, panel assembly, crosswalk bridges, reconciliation truth
  table (stale_carryforward / real_update / resampled_stable / ...).
- R/measure.R, R/decomposition.R — bring-your-own-measure validation,
  employment-weighted aggregation, sensitivity grids, shift-share
  decomposition.
- R/data_updates.R — official O*NET longitudinal update record.

## Non-negotiables

- CRAN: no unconditional network in tests/examples/vignettes; fixtures under
  inst/extdata and tests/testthat/fixtures.
- Never mock above the JSON-parse layer when a realistic-body fixture can
  exercise it (see tests/testthat/test-parse-layer.R).
- Windows dev quirk: do not pipe `Rscript -e` output; write a script file.
```

- [ ] **Step 6: Run** `Rscript -e 'devtools::build_readme(); devtools::test()'` → PASS.

- [ ] **Step 7: Commit**

```bash
git add vignettes/ README.Rmd README.md cran-comments.md inst/REFERENCES.bib .github/instructions/onet2r.instructions.md
git commit -m "Add methodology citations, ecosystem positioning, agent module map"
```

---

### Task 16: Version bump, NEWS, and the final gate

**Files:**
- Modify: `DESCRIPTION`, `NEWS.md`, `cran-comments.md`

- [ ] **Step 1: Bump `DESCRIPTION` Version to `0.4.2`.**

- [ ] **Step 2: NEWS.md** — add at the top:

```markdown
# onet2r 0.4.2

## Bug fixes

* Fixed a regression that broke `onet_crosswalk_bridge()` downloads
  (`download_crosswalk_file()` was not defined at package level).
* `onet_occupation_details()` now returns a tibble of section titles and
  URLs instead of an unnamed list.
* `onet_panel_reconcile()` aborts when release dates are missing instead of
  silently mis-ordering comparisons.

## Improvements

* Reconciliation now reports occupations missing from the crosswalk bridge
  (`coverage_status` values `"unmapped_source"` / `"unmapped_target"`).
* `onet_change_summary()` gains crosswalk-weighted counts (`n_weighted`,
  `share_weighted`).
* `onet_releases()` is memoised per session, reports archive `format`, and
  documents the text-archive floor (20.1, October 2015).
* Archive tables with unrecognized layouts now abort instead of returning
  all-NA panels.
* OEWS `annual`/`hourly` flags are parsed as logicals and wage-field
  semantics are documented.
* `onet_measure_aggregate()` reports large unmatched weight-panel
  employment shares.

## New features

* `onet_data_updates()` downloads the official O*NET Longitudinal Data
  Updates record — ground truth for which occupations were re-rated in each
  cycle.
```

- [ ] **Step 3: Full check**

Run each as a script file, not piped `-e`:

1. `devtools::test()` → `[ FAIL 0 | WARN 0 | ... ]`
2. `rcmdcheck::rcmdcheck(args = "--as-cran")` → 0 errors, 0 warnings; notes limited to new-submission boilerplate.
3. `urlchecker::url_check()` → no failures.
4. The live crosswalk-bridge smoke script from Task 1 Step 5 → `OK`. **Mandatory** — it is the one path the mocked suite cannot cover.
5. `pkgdown::check_pkgdown()` → OK (new `onet_data_updates` reference entry present).

- [ ] **Step 4: Update `cran-comments.md`** test-environments with the platforms actually run (add win-builder devel via `devtools::check_win_devel()` and at least one Linux check via the GitHub Actions R-CMD-check workflow results).

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION NEWS.md cran-comments.md
git commit -m "Prepare 0.4.2: remediation release notes and final checks"
```

---

## Completion checklist (maps to the spec's acceptance criteria)

- [ ] `onet_crosswalk_bridge("2010", "2019")` succeeds on a clean cache with live network (Tasks 1, 16).
- [ ] All P1 items closed with regression tests (Tasks 2-8).
- [ ] All P2 items closed (Tasks 9-12, 15-16) or deferred with rationale recorded in NEWS.
- [ ] P3 features implemented; the OEWS-combination premise verified before implementation (Tasks 13-14).
- [ ] `rcmdcheck --as-cran`: 0 errors, 0 warnings.
- [ ] No unconditional network access at check time (spot-check: `grep -rn "download.file\|readLines" tests/ vignettes/` shows only mocked or fixture-driven uses).
