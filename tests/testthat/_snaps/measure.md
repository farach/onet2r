# onet_task_to_occupation rejects multiple releases

    Code
      onet_task_to_occupation(measure, ratings)
    Condition
      Error in `validate_task_rating_grain()`:
      ! `task_ratings` must contain exactly one non-missing release per call.
      x Release columns `release_version` contain "29.0, 30.0" and 0 missing entries.
      i Pass one release per call. For multi-vintage analysis, pass a named list of single-release `task_ratings` data frames to `onet_measure_sensitivity()`.

# onet_task_to_occupation rejects duplicate rows within a release

    Code
      onet_task_to_occupation(measure, ratings)
    Condition
      Error in `validate_task_rating_grain()`:
      ! `task_ratings` must contain at most one row per occupation-task key after task and scale filters.
      x Duplicate key: onet_soc_code=15-1252.00, task_id=1.
      i Pass one release per call without duplicate task rows. For multi-vintage analysis, pass a named list of single-release `task_ratings` data frames to `onet_measure_sensitivity()`.

