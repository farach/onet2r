tasknet_fixture <- function() {
  system.file("extdata", "tasknet-synthetic.csv", package = "onet2r")
}

tasknet_raw <- function() {
  utils::read.csv(
    tasknet_fixture(), colClasses = "character", check.names = FALSE,
    na.strings = character()
  )
}

write_tasknet_test <- function(data) {
  path <- tempfile(fileext = ".csv")
  withr::defer(unlink(path), envir = parent.frame())
  utils::write.csv(data, path, row.names = FALSE, na = "")
  path
}

test_that("local reader preserves study identifiers, ratings and provenance", {
  tasks <- onet_tasknet_read(
    tasknet_fixture(), source_url = "https://example.org/tasknet.csv",
    as_of = "synthetic fixture"
  )
  expect_s3_class(tasks, "tbl_df")
  expect_equal(nrow(tasks), 9L)
  expect_equal(table(tasks$inventory), table(c(rep("TaskNet", 5), rep("O*NET", 4))))
  expect_identical(tasks$study_task_id, tasknet_raw()$id)
  expect_identical(tasks$onet_soc_code, tasknet_raw()$soc)
  expect_false(any(c("task_id", "release_version", "task_type") %in% names(tasks)))
  expect_equal(tasks$relevance, c(0.8, 1, 0.5, 0.25, 0.9, 0.6, 0.4, 0.75, 0.8))
  expect_equal(tasks$importance, c(4, 5, 3, 2, 4, 3, 2, 4, 4))
  expect_type(tasks$duration, "double")
  expect_true(all(is.na(tasks$duration)))
  expect_identical(unique(tasks$source_sha256), onet2r:::onet_sha256(tasknet_fixture()))
  expect_identical(unique(tasks$source_url), "https://example.org/tasknet.csv")
  expect_identical(unique(tasks$source_as_of), "synthetic fixture")
  expect_true(all(nzchar(tasks$source_read_at)))
  expect_true(all(file.exists(tasks$source_path)))
})

test_that("canonical relations preserve many-to-many links without doubling inverses", {
  links <- onet_tasknet_relations(onet_tasknet_read(tasknet_fixture()))
  expect_s3_class(links, "tbl_df")
  expect_equal(nrow(links), 5L)
  expect_equal(sum(links$relation_type == "refines"), 3L)
  expect_equal(sum(links$relation_type == "overlaps"), 2L)
  actual <- paste(links$from_study_task_id, links$to_study_task_id, links$relation_type)
  expect_setequal(actual, c(
    "S-FIX-01 O-FIX-01 refines",
    "S-FIX-02 O-FIX-01 refines",
    "S-FIX-02 O-FIX-02 refines",
    "S-FIX-03 O-FIX-02 overlaps",
    "S-AUX-01 O-AUX-01 overlaps"
  ))
  expect_equal(
    length(unique(links$from_study_task_id[links$relation_type == "refines"])),
    2L
  )
  expect_true(all(links$from_inventory == "TaskNet"))
  expect_true(all(links$to_inventory == "O*NET"))
  expect_identical(unique(links$source_sha256), onet2r:::onet_sha256(tasknet_fixture()))
})

test_that("expected stylized comparison values are reproduced", {
  tasks <- onet_tasknet_read(tasknet_fixture())
  expect_equal(mean(tasks$importance[tasks$inventory == "TaskNet"]), 3.6)
  expect_equal(mean(tasks$importance[tasks$inventory == "O*NET"]), 3.25)
  expect_equal(sum(tasks$region == "new"), 1L)
  expect_equal(sum(tasks$region == "missing"), 1L)
})

test_that("empty data and relation-free data have stable typed schemas", {
  full <- onet_tasknet_read(tasknet_fixture())
  empty <- onet_tasknet_read(write_tasknet_test(tasknet_raw()[0, ]))
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(full))
  expect_identical(vapply(empty, typeof, ""), vapply(full, typeof, ""))
  empty_links <- onet_tasknet_relations(empty)
  full_links <- onet_tasknet_relations(full)
  expect_equal(nrow(empty_links), 0L)
  expect_identical(names(empty_links), names(full_links))
  expect_identical(vapply(empty_links, typeof, ""), vapply(full_links, typeof, ""))

  raw <- tasknet_raw()[4, ]
  none <- onet_tasknet_relations(onet_tasknet_read(write_tasknet_test(raw)))
  expect_identical(names(none), names(full_links))
  expect_equal(nrow(none), 0L)
})

test_that("reader rejects missing, remote and invalid paths without network access", {
  expect_error(onet_tasknet_read(NULL), class = "rlang_error")
  expect_error(onet_tasknet_read(""), class = "rlang_error")
  expect_error(onet_tasknet_read(tempfile()), "exist", class = "rlang_error")
  expect_error(onet_tasknet_read(tempdir()), "file", class = "rlang_error")
  expect_error(onet_tasknet_read("https://example.org/data.csv"), "local", class = "rlang_error")
  expect_error(onet_tasknet_read("file:///data.csv"), "local", class = "rlang_error")
  expect_error(onet_tasknet_read(tasknet_fixture(), expected_sha256 = strrep("0", 64)), "SHA-256")
})

test_that("schema and identity defects fail explicitly", {
  raw <- tasknet_raw()
  missing <- raw
  missing$id <- NULL
  expect_error(onet_tasknet_read(write_tasknet_test(missing)), "id", class = "rlang_error")
  duplicate <- raw
  names(duplicate)[2] <- "id"
  expect_error(onet_tasknet_read(write_tasknet_test(duplicate)), "column", class = "rlang_error")
  for (column in c("id", "occupation", "soc", "statement", "inventory", "region")) {
    bad <- raw
    bad[[column]][1] <- " "
    expect_error(
      onet_tasknet_read(write_tasknet_test(bad)),
      class = "rlang_error", info = column
    )
  }
  bad <- raw
  bad$id[2] <- bad$id[1]
  expect_error(onet_tasknet_read(write_tasknet_test(bad)), "unique|Duplicate|duplicate")
  bad <- raw
  bad$soc[1] <- "15-1252"
  expect_error(onet_tasknet_read(write_tasknet_test(bad)), "soc|SOC")
  bad <- raw
  bad$inventory[1] <- "Other"
  expect_error(onet_tasknet_read(write_tasknet_test(bad)), "inventory")
  bad <- raw
  bad$region[1] <- "missing"
  expect_error(onet_tasknet_read(write_tasknet_test(bad)), "region")
  bad <- raw
  bad$source_sha256 <- "forged"
  expect_error(onet_tasknet_read(write_tasknet_test(bad)), "reserved|collision")
})

test_that("numeric ratings are strict, bounded and never silently defaulted", {
  for (token in c("4x", "1,000", "0x10", "1e309", "NaN", "Inf", "-Inf")) {
    raw <- tasknet_raw()
    raw$importance[1] <- token
    expect_error(
      onet_tasknet_read(write_tasknet_test(raw)), "importance",
      class = "rlang_error", info = token
    )
  }
  for (column in c("relevance", "articulability", "verifiability", "top_worker_gap")) {
    raw <- tasknet_raw()
    raw[[column]][1] <- "1.01"
    expect_error(onet_tasknet_read(write_tasknet_test(raw)), column)
  }
  raw <- tasknet_raw()
  raw$importance[1] <- "5.01"
  expect_error(onet_tasknet_read(write_tasknet_test(raw)), "importance")
  raw$importance[1] <- "0"
  expect_error(onet_tasknet_read(write_tasknet_test(raw)), "importance")
  raw <- tasknet_raw()
  raw$frequency[1] <- "7.01"
  expect_error(onet_tasknet_read(write_tasknet_test(raw)), "frequency")
  for (column in c("duration", "training_svp")) {
    raw <- tasknet_raw()
    raw[[column]][1] <- "9.01"
    expect_error(onet_tasknet_read(write_tasknet_test(raw)), column)
    raw[[column]][1] <- "8.75"
    expect_equal(onet_tasknet_read(write_tasknet_test(raw))[[column]][1], 8.75)
  }
  raw <- tasknet_raw()
  raw$relevance[1] <- ""
  raw$importance[1] <- ""
  tasks <- onet_tasknet_read(write_tasknet_test(raw))
  expect_true(is.na(tasks$relevance[1]))
  expect_true(is.na(tasks$importance[1]))
})

test_that("optional fields have stable types and unknown fields are preserved", {
  raw <- tasknet_raw()
  raw$duration <- NULL
  raw$themes <- NULL
  raw$custom_annotation <- "source note"
  tasks <- onet_tasknet_read(write_tasknet_test(raw))
  expect_type(tasks$duration, "double")
  expect_true(all(is.na(tasks$duration)))
  expect_type(tasks$themes, "character")
  expect_identical(tasks$custom_annotation, rep("source note", 9L))
  expect_type(tasks$eloundou_beta, "character")
})

test_that("missing, malformed and misdirected relation targets fail closed", {
  defects <- list(
    dangling = c("refines", "O-ABSENT"),
    self = c("refines", "S-FIX-01"),
    same_inventory = c("refines", "S-FIX-02"),
    cross_occupation = c("refines", "O-AUX-01"),
    wrong_direction = c("refined_by", "O-FIX-01"),
    empty_token = c("refines", "O-FIX-01;"),
    duplicate_token = c("refines", "O-FIX-01; O-FIX-01")
  )
  for (defect in names(defects)) {
    raw <- tasknet_raw()
    raw[[defects[[defect]][1]]][1] <- defects[[defect]][2]
    expect_error(
      onet_tasknet_read(write_tasknet_test(raw)),
      class = "rlang_error", info = defect
    )
  }
  tasks <- onet_tasknet_read(tasknet_fixture())
  expect_error(
    onet_tasknet_relations(tasks[tasks$inventory == "TaskNet", ]),
    "modified|unchanged|target|endpoint|dangling", class = "rlang_error"
  )
  tasks$refines[1] <- "O-ABSENT"
  expect_error(onet_tasknet_relations(tasks), class = "rlang_error")
})

test_that("CSV structure, UTF-8 BOM and quoted multiline fields are handled", {
  raw <- tasknet_raw()
  raw$statement[1] <- "Stylized: discuss \"quoted\", comma-separated\nfixture text."
  path <- write_tasknet_test(raw)
  expect_identical(onet_tasknet_read(path)$task[1], raw$statement[1])

  bom <- tempfile(fileext = ".csv")
  withr::defer(unlink(bom))
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), bytes), bom)
  expect_identical(onet_tasknet_read(bom)$task, raw$statement)
  raw$statement[1] <- "Stylized: discuss a caf\u00e9 fixture."
  expect_identical(
    onet_tasknet_read(write_tasknet_test(raw))$task[1], raw$statement[1]
  )

  malformed <- tempfile(fileext = ".csv")
  withr::defer(unlink(malformed))
  writeLines(c(paste(names(raw), collapse = ","), "too,few,fields"), malformed)
  expect_error(onet_tasknet_read(malformed), class = "rlang_error")
  writeLines(c(paste(names(raw), collapse = ","), paste(rep("x", 23), collapse = ",")), malformed)
  expect_error(onet_tasknet_read(malformed), class = "rlang_error")
  writeLines(c(paste(names(raw), collapse = ","), '"unclosed,quote'), malformed)
  expect_error(onet_tasknet_read(malformed), class = "rlang_error")
  writeLines(character(), malformed)
  expect_error(onet_tasknet_read(malformed), class = "rlang_error")
})

test_that("whitespace, source metadata and inverse declarations are explicit", {
  raw <- tasknet_raw()
  raw$refines[2] <- " O-FIX-01 ; O-FIX-02 "
  raw$relevance[1] <- " 0.8 "
  tasks <- onet_tasknet_read(write_tasknet_test(raw))
  expect_equal(tasks$relevance[1], 0.8)
  expect_equal(nrow(onet_tasknet_relations(tasks)), 5L)
  expect_true(all(is.na(tasks$source_url)))
  expect_true(all(is.na(tasks$source_as_of)))
  expect_error(
    onet_tasknet_read(tasknet_fixture(), source_url = "not a URL"),
    class = "rlang_error"
  )

  raw <- tasknet_raw()
  raw$refined_by[5] <- "S-FIX-01"
  expect_error(onet_tasknet_read(write_tasknet_test(raw)), "reciprocal|inverse|declaration")
})

test_that("relation helper validates inputs rather than trusting reader attributes", {
  expect_error(onet_tasknet_relations(NULL), class = "rlang_error")
  expect_error(onet_tasknet_relations(tibble::tibble()), class = "rlang_error")
  tasks <- onet_tasknet_read(tasknet_fixture())
  tasks$study_task_id[2] <- tasks$study_task_id[1]
  expect_error(onet_tasknet_relations(tasks), class = "rlang_error")
  tasks <- onet_tasknet_read(tasknet_fixture())
  tasks$source_sha256[1] <- strrep("0", 64)
  expect_error(onet_tasknet_relations(tasks), class = "rlang_error")
  tasks <- onet_tasknet_read(tasknet_fixture())
  tasks$importance[1] <- 1
  expect_error(onet_tasknet_relations(tasks), class = "rlang_error")
  tasks <- onet_tasknet_read(tasknet_fixture())
  receipt <- attr(tasks, "source_receipt")
  receipt$actual_sha256 <- "forged"
  attr(tasks, "source_receipt") <- receipt
  expect_error(onet_tasknet_relations(tasks), class = "rlang_error")
})

test_that("source receipts are redacted and timestamps describe reading", {
  tasks <- onet_tasknet_read(
    tasknet_fixture(),
    source_url = "https://example.org/data.csv?token=synthetic-secret",
    as_of = as.Date("2026-01-01")
  )
  expect_false(any(grepl("synthetic-secret", tasks$source_url, fixed = TRUE)))
  expect_identical(unique(tasks$source_as_of), "2026-01-01")
  expect_identical(tasks$source_record, seq_len(9L))
  expect_match(unique(tasks$source_read_at), "Z$")
  expect_identical(
    attr(tasks, "source_receipt")$actual_sha256,
    unique(tasks$source_sha256)
  )
  expect_silent(onet_tasknet_read(
    tasknet_fixture(),
    expected_sha256 = toupper(onet2r:::onet_sha256(tasknet_fixture()))
  ))
  expect_error(
    onet_tasknet_read(tasknet_fixture(), expected_sha256 = "bad"),
    class = "rlang_error"
  )
})

test_that("parsed bytes stay bound to the private snapshot and it is cleaned up", {
  path <- write_tasknet_test(tasknet_raw())
  original_digest <- onet2r:::onet_sha256(path)
  snapshot <- onet2r:::onet_local_source_snapshot
  captured <- NULL
  local_mocked_bindings(onet_local_source_snapshot = function(...) {
    captured <<- snapshot(...)
    changed <- tasknet_raw()
    changed$importance[1] <- "1"
    utils::write.csv(changed, path, row.names = FALSE, na = "")
    captured
  })
  tasks <- onet_tasknet_read(path, expected_sha256 = original_digest)
  expect_equal(tasks$importance[1], 4)
  expect_identical(unique(tasks$source_sha256), original_digest)
  expect_false(file.exists(captured))
  changed <- onet_tasknet_read(path)
  expect_equal(changed$importance[1], 1)
  expect_false(identical(unique(changed$source_sha256), original_digest))
  expect_false(file.exists(captured))
})

test_that("failed parsing also cleans the private snapshot", {
  raw <- tasknet_raw()
  raw$importance[1] <- "invalid"
  path <- write_tasknet_test(raw)
  snapshot <- onet2r:::onet_local_source_snapshot
  captured <- NULL
  local_mocked_bindings(onet_local_source_snapshot = function(...) {
    captured <<- snapshot(...)
    captured
  })
  expect_error(onet_tasknet_read(path), "importance")
  expect_false(file.exists(captured))
})

test_that("bare balanced quotes are rejected rather than silently removed", {
  path <- tempfile(fileext = ".csv")
  withr::defer(unlink(path))
  lines <- readLines(tasknet_fixture())
  lines[2] <- sub(
    "Stylized: prepare a fixture plan.",
    'Stylized: ""prepare"" a fixture plan.',
    lines[2], fixed = TRUE
  )
  writeLines(lines, path)
  expect_error(onet_tasknet_read(path), "quot", class = "rlang_error")
})

test_that("typed edges are distinct from statement pairs and round trips preserve integrity", {
  raw <- tasknet_raw()
  raw$overlaps[1] <- "O-FIX-01"
  raw$overlaps[5] <- "S-FIX-01"
  tasks <- onet_tasknet_read(write_tasknet_test(raw))
  links <- onet_tasknet_relations(tasks)
  expect_equal(nrow(links), 6L)
  expect_equal(nrow(unique(links[c("from_study_task_id", "to_study_task_id")])), 5L)
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(tasks, path)
  expect_identical(onet_tasknet_relations(readRDS(path)), links)
})
