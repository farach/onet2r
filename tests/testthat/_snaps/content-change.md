# content_change rejects duplicate filtered grain

    Code
      onet_content_change(panel)
    Condition
      Error in `onet_content_change()`:
      ! `panel` must contain at most one row per occupation, item, release, and scale after filtering.
      x Duplicate key: onet_soc_code=15-1132.00, task_id=1, release_version=22.1, scale_id=IM.
      i Remove the duplicate rows or aggregate them explicitly before calling `onet_content_change()`.

