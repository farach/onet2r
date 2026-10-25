Derived public-data snapshot for "What Makes Occupations Different?"

Source: O*NET 30.3 Database, U.S. Department of Labor, Employment and
Training Administration (USDOL/ETA).
https://www.onetcenter.org/dl_files/database/db_30_3_text.zip
License: Creative Commons Attribution 4.0 International.
https://creativecommons.org/licenses/by/4.0/
O*NET is a trademark of USDOL/ETA.

onet2r has modified this information by calculating occupation-DWA document
frequencies, normalized binary-presence TF-IDF summaries, total-variation
profile distances, coverage counts, hierarchy-resolution summaries,
information-loss accounting, and finite-run fixed-margin randomizations.
Occupation titles and activity descriptions are retained as source labels.
USDOL/ETA has not approved, endorsed, or tested these modifications.

Regenerate from the repository root with the current package installed:
Rscript data-raw\occupational-distinctiveness.R path\to\db_30_3_text.zip
Rscript data-raw\occupational-information.R path\to\db_30_3_text.zip
Rscript data-raw\occupational-null.R path\to\db_30_3_text.zip

provenance.csv records the source URL, SHA-256, snapshot date, universe,
inclusion rule, and weighting choices. The raw archive is not bundled.
coverage.csv retains taxonomy entries outside the task-based universe,
with blank task counts rather than invented zero-work profiles.
occupation-profiles.csv rounds rarity scores and pairwise distances to 12
decimal places before ranking or choosing neighbors, so floating-point
noise does not break ties. All tied nearest occupations are retained,
separated by semicolons in nearest_occupations.
resolution-summary.csv aggregates the same binary DWA mass to its published
IWA and GWA parents, without re-binarizing or refitting IDF.
No employment weighting or task-rating weighting is applied.

information-summary.csv fixes a uniform occupation prior, then samples a
linked DWA uniformly within the selected occupation. The activity mixture
is the mean normalized profile, not df/N. IWA and GWA aggregation preserves
that original mass. information-loss.csv attributes fine-to-generalized
loss to GWA parents, carrying both mixture mass and conditional loss.
information-occupation-diagnostics.csv is diagnostic, not a new leaderboard.
Generalized activity names come from Content Model Reference.
task-inventory.csv preserves Core/Supplemental counts and record-date ranges
so the leader caveats and the 893-occupation Core-only support are inspectable.

null-summary.csv records observed draw 0 and 20 randomized endpoints at
each grain. null-size-summary.csv records the corresponding size/rarity
Spearman correlations. Each Curveball run restarts from the observed binary
matrix and attempts 20*N two-row trades; N=923. One initial seed, 20261025,
supplies successive pseudorandom draws and is not reset at each restart.
Every occupation size and every DWA frequency is preserved exactly.
These endpoints are descriptive references, not certified uniform samples.
Their range is not a confidence interval. No p-values are calculated.

The baseline includes Supplemental tasks. Its rarity leader has no Core
tasks in this archive. A literal Core-only restriction changes support.
The May 2026 release includes records of mixed earlier vintages.
Information loss changes database labels, not jobs or worker capabilities.
Carry this attribution/license/modification notice with separate CSV copies.
