# onet_task_to_occupation rejects conflicting release columns

    Code
      onet_task_to_occupation(measure, ratings)
    Condition
      Error in `task_rating_release_values()`:
      ! `task_ratings` has conflicting release provenance.
      x Conflicting row: row=1, release_version=30.0, version=29.0.
      i Make `release_version` and `version` agree, or leave one missing.

# onet_task_to_occupation rejects multiple releases

    Code
      onet_task_to_occupation(measure, ratings)
    Condition
      Error in `validate_task_rating_grain()`:
      ! `task_ratings` must contain exactly one non-missing release per call.
      x Effective release values are "29.0, 30.0" with 0 missing rows.
      i Pass one release per call. For multi-vintage analysis, pass a named list of single-release `task_ratings` data frames to `onet_measure_sensitivity()`.

# onet_task_to_occupation rejects duplicate rows within a release

    Code
      onet_task_to_occupation(measure, ratings)
    Condition
      Error in `validate_task_rating_grain()`:
      ! `task_ratings` must contain at most one row per occupation-task key after task and scale filters.
      x Duplicate key: onet_soc_code=15-1252.00, task_id=1.
      i Pass one release per call without duplicate task rows. For multi-vintage analysis, pass a named list of single-release `task_ratings` data frames to `onet_measure_sensitivity()`.

