# onet2r Remediation & Completion — Design

**Date:** 2026-07-02
**Status:** Approved
**Baseline:** main @ a80bfa6 (v0.4.1 + "Prepare CRAN audit fixes" 5277a15)
**Companion:** `2026-07-02-onet2r-remediation-implementation.md` (the executable plan)

## Goal

Bring onet2r to a defensible CRAN submission by (1) fixing one blocking regression, (2) closing the remaining findings from a five-track audit (live API verification, four module audits, one external-methodology research pass), and (3) adding four research-driven features that close real gaps for the target audience: labor economists building occupation-level measures.

## How this design was derived

Five independent evidence streams, 2026-07-02:

1. **Live API verification** against api-v2.onetcenter.org with a real key (24 endpoints).
2. **Four module audits** (core wrapper; OEWS; PUMS/weights; longitudinal panel) — the panel audit downloaded real archives (db_20_1, db_30_3) and live-verified crosswalk URLs and taxonomy dates; the PUMS audit executed the code and confirmed bugs empirically.
3. **Deep research** on longitudinal O*NET methodology, OEWS practice, PUMS practice, and the R ecosystem (adversarially verified where session limits allowed; unverified claims are marked as such in the implementation plan and must be re-verified during implementation).
4. **Gap analysis** classifying every audit finding against HEAD after the maintainer's fix commit: roughly three-quarters verified FIXED with file:line evidence; 12 PARTIAL and 7 OPEN (grouped into P1/P2 below); 1 new blocking regression introduced by the fix commit itself.
5. **Ecosystem inventory**: ONETr (CRAN) wraps the retired v1 XML API — onet2r is the modern successor; blscrapeR wraps the BLS time-series API and is complementary (it cannot serve OEWS research flat files); tidycensus/ipumsr are the sanctioned PUMS fetchers (onet2r correctly does not fetch); **no longitudinal O*NET panel tooling exists in R or Python** — onet2r's panel/reconcile machinery is its unique contribution.

## Standing decisions (maintainer-approved)

- **No code edits during planning** — the implementation plan is the single source of truth; a separate coding agent executes it.
- **Integrate, don't reimplement** — overlaps with healthy packages become interop points (tidycensus for PUMS; documented manual-download escape hatches for BLS).
- **CRAN is the target** — no unconditional network in tests/examples/vignettes; caching only under `tools::R_user_dir("onet2r", "cache")` with a working clear path; graceful failure on unavailable resources.
- **Bring-your-own-measure** — the package ships plumbing and audit machinery, never substantive measures.

## Scope

### P0 — Blocking regression (release gate)

`download_crosswalk_file()` was pasted *inside* `read_adjacent_crosswalk()` after its call site (R/panel.R). Any real (uncached, unmocked) `onet_crosswalk_bridge()` call fails with `could not find function "download_crosswalk_file"`. Live-confirmed at HEAD. The 342-test suite passes because both bridge tests mock `read_adjacent_crosswalk` — mocks sit above the broken layer. Fix: hoist to top level; add a test that mocks only the network primitive so the full parse path runs.

### P1 — Open correctness fixes (7)

1. `onet_occupation_details()` returns an unnamed 17-element list (live-confirmed) — parse into named sections.
2. Reconcile drops from-release occupations absent from the bridge with no accounting — add `unmapped_source` coverage status; separate bridge-gap "new" from genuinely-new.
3. `onet_change_summary()` ignores `crosswalk_weight` — weight the summary or document "unweighted" explicitly.
4. NA `release_date` sorts silently in reconcile ordering — abort or warn.
5. OEWS ANNUAL/HOURLY flag columns uncoerced and wage-field semantics undocumented.
6. `latest_oews_year()` hardcodes an April cutoff — document and provide fallback guidance.
7. Dead code: `extract_paged_data()`, `onet_first_list_key()`.

### P2 — Partials to close (~12)

Parse-layer JSON fixtures for detail endpoints; fixture realism (spaced archive filenames, drop the nonexistent Title column, recorded releases-HTML snippet); `onet_releases()` format column + loud pre-10.0 refusal + documented auto-download floor + per-session memoisation (also fixes the double network hit in `onet_archive_read()`); `skip_on_cran()`/`skip_if_offline()` in smoke tests; cache-expiry documentation; census.R CPS-claim scoping + 5-year PUMS caveat; stale-carryforward semantics in decomposition docs; military-crosswalk docs + range validation; `setwd()` teardown fix in the panel test helper; version bump + NEWS + cran-comments with multi-platform check results; `.github/instructions/onet2r.instructions.md` enriched with the module map and conventions.

### P3 — Research-driven features (4)

1. **`onet_data_updates()`** — ingest O*NET's official *Longitudinal Data Updates* XLSX (per-release re-rating record; verified 3-0). Ground truth to complement the inferred change truth table; join helper + reconcile integration + vignette section.
2. **OEWS aggregation-code diagnostics** — May 2021+ OEWS omits some detailed SOCs and publishes OEWS-specific combinations; surface unmatched-code diagnostics in the weight-panel path. *Claim unverified (session limit) — implementation must verify against bls.gov first.*
3. **Vignette enrichment** — analyst prior-ratings dependence since Cycle 12 (O*NET AOSkills procedure report); 2008 Skills incumbent→analyst break citation; IM/LV near-redundancy (Handel 2016, mean r ≈ 0.92) note in the measure vignette.
4. **Ecosystem positioning** — README/cran-comments paragraph on ONETr (retired v1 API), blscrapeR (complementary), tidycensus (sanctioned PUMS fetcher).

## Non-goals

- No Census/PUMS fetcher (tidycensus's job).
- No employment-weighted crosswalk bridges until a sourced weight exists (current loud refusal is correct).
- No bundled substantive measures.
- No pre-5.0 (2003) archive support — no survey-based data exists before the Data Collection Program.
- No OEWS API client — research-grade OEWS exists only as flat files.

## Implementation-plan requirements

The companion plan must be executable by a coding agent with **no access to this conversation**:

- Global preamble: repo layout, module map, CRAN constraints, tidyverse style rules, and the local verification quirk (piped `Rscript -e` swallows output on this Windows setup — always write a script file and run it).
- Per task: context/why (with audit evidence), exact files and lines, the change specification, the test to add, and a verification command with expected output.
- Final gate: `devtools::test()` (0 failures), `rcmdcheck::rcmdcheck(args = "--as-cran")` (0 errors/warnings), and a **live crosswalk-bridge smoke test** — the one check the mocked suite cannot provide.

## Acceptance criteria

1. `onet_crosswalk_bridge("2010", "2019")` succeeds on a clean cache with live network.
2. All P1 items closed with regression tests; all P2 items closed or explicitly deferred with rationale in NEWS.
3. P3 features implemented (or, for the OEWS-combination item, verified-then-implemented or verified-then-dropped with a documented reason).
4. R CMD check --as-cran: 0 errors, 0 warnings, notes limited to new-submission boilerplate.
5. No test, example, or vignette touches the network unconditionally at check time.
