# onet_archive_read() parses archives with data.table::fread(). These tests
# pin its parsing to the utils::read.delim() rules the package used before,
# so a data.table release or a user's data.table options cannot change results
# unnoticed. Inputs are ASCII so the comparison holds in any session encoding.

write_archive_file <- function(lines, eol = "\n") {
  path <- withr::local_tempfile(fileext = ".txt", .local_envir = parent.frame())
  bytes <- charToRaw(enc2utf8(paste0(paste(lines, collapse = eol), eol)))
  writeBin(bytes, path)
  path
}

read_delim_reference <- function(path, as_text = FALSE) {
  utils::read.delim(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    quote = "",
    na.strings = c("NA", "n/a", "N/A"),
    colClasses = if (as_text) "character" else NA
  )
}

tricky_archive_lines <- function() {
  c(
    paste(
      "O*NET-SOC Code", "Task ID", "Task", "Data Value", "N",
      "Recommend Suppress", "Date", "Note", "Empty", "Updated", "Released",
      "2025",
      sep = "\t"
    ),
    paste(
      "15-1252.00", "01001", "Say \"hi\" to users.", "4.50", "27", "N",
      "07/2025", "2025-07-01 10:00:00", "NA", "2025-07-01 10:00:00",
      "2025-07-01", "1",
      sep = "\t"
    ),
    paste(
      " 29-1141.00 ", "1002", "\"unbalanced quote", "", "n/a", "Y",
      "", "1,5", "N/A", "2024-08-01 11:30:00", "2024-08-01", "2",
      sep = "\t"
    ),
    "",
    paste(
      "11-1011.00", "1003", "Big and special numbers", "1e5",
      "12345678901", "N", "2025-07-01", "Inf", "NA",
      "2023-01-15 09:00:00", "2023-01-15", "3",
      sep = "\t"
    ),
    "11-1011.00\t1004\tShort row"
  )
}

test_that("the archive reader parses text like read.delim", {
  for (eol in c("\n", "\r\n")) {
    path <- write_archive_file(tricky_archive_lines(), eol = eol)

    expect_identical(
      onet2r:::read_archive_text(path),
      read_delim_reference(path)
    )
    expect_identical(
      onet2r:::read_archive_text(path, as_text = TRUE),
      read_delim_reference(path, as_text = TRUE)
    )
  }

  # Year-like column names must stay names, not become a data row.
  years <- write_archive_file(c("2024\t2025", "1\t2", "3\t4"))
  expect_identical(
    onet2r:::read_archive_text(years),
    read_delim_reference(years)
  )
})

test_that("the archive reader ignores data.table options", {
  path <- write_archive_file(c(
    "Code\tFlag\tBinary\tID\tCount\tBlank",
    "a\tY\t0\t007\t12345678901\t",
    "b\tN\t1\t008\t2\tx"
  ))
  expected <- onet2r:::read_archive_text(path)

  withr::local_options(list(
    datatable.na.strings = "",
    datatable.integer64 = "character",
    datatable.logical01 = TRUE,
    datatable.logicalYN = TRUE,
    datatable.keepLeadingZeros = TRUE,
    datatable.fread.datatable = TRUE,
    datatable.showProgress = TRUE,
    datatable.verbose = TRUE
  ))

  expect_silent(result <- onet2r:::read_archive_text(path))
  expect_identical(result, expected)
  expect_identical(result$Flag, c("Y", "N"))
})

test_that("the archive reader does not depend on the session time zone", {
  path <- write_archive_file(tricky_archive_lines())
  read_in <- function(tz) {
    withr::local_timezone(tz)
    onet2r:::read_archive_text(path)
  }

  utc <- read_in("UTC")
  expect_identical(read_in("America/New_York"), utc)
  expect_identical(utc$Updated, read_delim_reference(path)$Updated)
  expect_type(utc$Released, "character")
})

test_that("onet_archive_read keeps UTF-8 text intact", {
  archive <- withr::local_tempdir()
  task <- "Review r\u00e9sum\u00e9s \u2013 caf\u00e9 menus"
  header <- c(
    "O*NET-SOC Code", "Task ID", "Task", "Task Type",
    "Incumbents Responding", "Date", "Domain Source"
  )
  row <- c("15-1252.00", "1001", task, "Core", "27", "07/2025", "Incumbent")
  text <- paste0(
    paste(header, collapse = "\t"), "\n",
    paste(row, collapse = "\t"), "\n"
  )
  writeBin(charToRaw(enc2utf8(text)), file.path(archive, "Task Statements.txt"))

  out <- onet_archive_read(
    "30.3",
    "Task Statements",
    path = archive,
    release_date = "2026-05-01"
  )

  expect_identical(enc2utf8(out$task), enc2utf8(task))
  expect_identical(out$source_date, as.Date("2025-07-01"))
})
