openai_iwa_fixture <- function() {
  system.file("extdata", "openai-iwa-synthetic.csv", package = "onet2r")
}

openai_iwa_write <- function(text) {
  path <- tempfile(fileext = ".csv")
  writeLines(text, path, useBytes = TRUE)
  path
}

test_that("synthetic shares preserve the source grain and expected values", {
  x <- onet_openai_iwa_read(openai_iwa_fixture(), as_of = "invented fixture")
  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 9L)
  expect_equal(length(unique(x$country_code)), 2L)
  expect_equal(length(unique(x$quarter)), 2L)
  expect_equal(sum(x$is_other_iwa), 3L)
  expect_equal(sum(x$share_of_messages == 0), 1L)
  expect_identical(x$quarter[1], "2025-01-01")
  expect_identical(x$source_record, 1:9)
  expect_true(all(is.na(x$iwa_id[x$is_other_iwa])))
  expect_identical(x$iwa_category[x$is_other_iwa], rep("Other IWA", 3))
  expect_equal(sum(x$share_of_messages[x$country_code == "US" &
    x$quarter == "2025-04-01"]), 0.99999)
  expect_false(any(x$country_code == "GB" & x$quarter == "2025-04-01"))
  expect_true(all(x$source_as_of == "invented fixture"))
})

test_that("header-only, BOM, quoted fields and reordered headers are supported", {
  header <- "quarter,country,iwa_cleaned,share_of_messages"
  path <- openai_iwa_write(header)
  on.exit(unlink(path), add = TRUE)
  empty <- onet_openai_iwa_read(path)
  full <- onet_openai_iwa_read(openai_iwa_fixture())
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(full))
  expect_identical(vapply(empty, typeof, ""), vapply(full, typeof, ""))
  writeLines(c(
    "\ufeffshare_of_messages,iwa_cleaned,country,quarter",
    '"0.25","4.A.1.a.1.I01","US","2025-01-01"'
  ), path, useBytes = TRUE)
  x <- onet_openai_iwa_read(path)
  expect_equal(x$share_of_messages, 0.25)
  expect_identical(x$country_code, "US")
})

test_that("invalid schema and CSV syntax fail explicitly", {
  texts <- c(
    "",
    "quarter,country,iwa_cleaned,share_of_messages\n2025-01-01,US,Other IWA,0.2,extra",
    "quarter,country,iwa_cleaned,share_of_messages\n2025-01-01,US,Other IWA",
    "quarter,country,iwa_cleaned,share_of_messages\n2025-01-01,US,Other \"IWA\",0.2",
    "quarter,country,iwa_cleaned,share_of_messages\n2025-01-01,US,\"Other IWA\"x,0.2",
    "quarter,country,iwa_cleaned,share_of_messages\n2025-01-01,US,\"Other IWA,0.2",
    "quarter,country,iwa_cleaned,share_of_messages\n\n2025-01-01,US,Other IWA,0.2",
    "quarter,country,iwa_cleaned,share_of_messages\n2025-01-01,US,Other IWA,0.2\n\n",
    "quarter,country,country,share_of_messages",
    "quarter,country,iwa_cleaned,unexpected",
    "quarter,country,iwa_cleaned,share_of_messages,extra"
  )
  for (text in texts) {
    path <- openai_iwa_write(text)
    expect_error(onet_openai_iwa_read(path), "OpenAI IWA")
    unlink(path)
  }
})

test_that("invalid keys, duplicate keys and invalid shares are rejected", {
  header <- "quarter,country,iwa_cleaned,share_of_messages"
  rows <- c(
    "2025-02-01,US,Other IWA,0.2",
    "2025-01-02,US,Other IWA,0.2",
    "2025-01-01,us,Other IWA,0.2",
    "2025-01-01,USA,Other IWA,0.2",
    "2025-01-01,US,-1,0.2",
    "2025-01-01,US,Other,0.2",
    "2025-01-01,US,4.A.1.a.1.D01,0.2",
    paste0("2025-01-01,US,Other IWA,", c("", "NA", "NaN", "Inf",
      "0x10", "1e309", "-0.01", "1.01", "20%", "0.2junk"))
  )
  for (row in rows) {
    path <- openai_iwa_write(c(header, row))
    expect_error(onet_openai_iwa_read(path), "OpenAI IWA")
    unlink(path)
  }
  path <- openai_iwa_write(c(header, rep("2025-01-01,US,Other IWA,0.2", 2)))
  on.exit(unlink(path), add = TRUE)
  expect_error(onet_openai_iwa_read(path), "keys must be unique")
})

test_that("local provenance is digest pinned and public URLs are redacted", {
  path <- openai_iwa_fixture()
  hash <- digest::digest(file = path, algo = "sha256")
  x <- onet_openai_iwa_read(
    path, expected_sha256 = hash,
    source_url = "https://example.org/data.csv?api_key=secret",
    as_of = "invented fixture"
  )
  expect_true(all(x$source_sha256 == hash))
  expect_false(grepl("secret", x$source_url[1], fixed = TRUE))
  receipt <- attr(x, "source_receipt", exact = TRUE)
  expect_identical(receipt$actual_sha256, hash)
  expect_identical(receipt$source_path, x$source_path[1])
  expect_error(onet_openai_iwa_read(path, expected_sha256 = paste(rep("0", 64), collapse = "")),
    "SHA-256|digest|match")
  expect_error(onet_openai_iwa_read("https://example.org/data.csv"), "local CSV")
  expect_error(onet_openai_iwa_read(tempdir()), "local CSV")
  expect_error(onet_openai_iwa_read(path, source_url = "file:///tmp/data.csv"), "HTTP")
})

test_that("invalid text bytes and a ZIP do not masquerade as a CSV", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  for (bytes in list(
    as.raw(c(0xff, 0xfe, 0x41)),
    as.raw(c(0x41, 0x00, 0x42)),
    as.raw(c(0x50, 0x4b, 0x03, 0x04))
  )) {
    writeBin(bytes, path)
    expect_error(onet_openai_iwa_read(path), "OpenAI IWA")
  }
})
