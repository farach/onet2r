#' Read Local OpenAI Signals Work-Activity Shares
#'
#' Reads the country-quarter Intermediate Work Activity (IWA) CSV from the
#' OpenAI Signals release. These are published shares of classified work-related
#' consumer ChatGPT messages, not occupation-level adoption or AI exposure.
#'
#' @param path Path to the extracted local UTF-8 CSV, not the download ZIP.
#' @param source_url Optional public HTTP(S) source URL supplied by the caller.
#' @param as_of Optional caller-supplied snapshot label.
#' @param expected_sha256 Optional SHA-256 digest of the extracted CSV bytes.
#'
#' @return A tibble with character `quarter`, `country_code`, `iwa_category`,
#'   and `iwa_id`, logical `is_other_iwa`, and numeric `share_of_messages`.
#'   `quarter` is the ISO date of the quarter's first day. `iwa_category`
#'   preserves the published code or `"Other IWA"` label. The latter has a
#'   missing `iwa_id`, not an invented O&#42;NET identifier. Provenance columns
#'   are `source_path`, `source_url`, `source_sha256`, `source_read_at`,
#'   `source_as_of`, and integer `source_record`. A full source receipt is
#'   retained as an attribute. Header-only files return the same typed schema.
#'
#' @details
#' Extract `share_of_work_related_messages_by_onet_iwa_country_quarter.csv`
#' from the manually downloaded release. The required, exclusive source
#' columns are `quarter`, `country`, `iwa_cleaned`, and `share_of_messages`.
#' Column order may vary. Duplicate country-quarter-category keys, malformed
#' CSV records, invalid identifiers, and nonfinite or out-of-range shares are
#' errors. Quarter dates must be January, April, July, or October 1.
#'
#' Shares are conditional on country, quarter, and classification as work
#' related. They are not shares of workers, employment, all messages, or time
#' spent at work. The reader does not normalize, aggregate, select an exposure
#' measure, or allocate usage to occupations. Partial country-quarter groups
#' are permitted and are not assumed complete. Unreported rows remain absent,
#' not zero. Reported zeros remain zero.
#'
#' The release README version 3.0 specifies five-decimal rounding for IWA
#' shares, after differential-privacy noise and normalization. Consequently,
#' published country-quarter totals can differ slightly from one. The
#' November 2026 download contains 164 named IWAs plus `"Other IWA"`, not
#' 165 individually identified IWAs. This is a snapshot fact, not a fixed
#' category count enforced by the reader.
#'
#' The source covers sampled adult consumer ChatGPT messages. Enterprise
#' messages and Codex are excluded, as are users who deleted their accounts
#' or opted out of training. Message classification does not establish the
#' user's occupation or successful completion of an activity. Linking IWAs
#' to occupational reference tables does not establish occupational adoption.
#'
#' The release README specifies CC BY 4.0. Cite OpenAI Signals and identify
#' the source snapshot when using the data. No real usage data are bundled.
#' The digest describes exactly the CSV bytes parsed, not its enclosing ZIP.
#' Source URL and `as_of` are caller assertions. Local read time is not
#' download time. See <https://openai.com/signals/data/> and the accompanying
#' README for methodology and licensing.
#'
#' @export
#' @examples
#' path <- system.file("extdata", "openai-iwa-synthetic.csv", package = "onet2r")
#' usage <- onet_openai_iwa_read(path, as_of = "synthetic example")
#' usage[c("quarter", "country_code", "iwa_category", "share_of_messages")]
onet_openai_iwa_read <- function(
    path,
    source_url = NULL,
    as_of = NULL,
    expected_sha256 = NULL) {
  validate_single_string(path, "path")
  if (!nzchar(trimws(path)) ||
    grepl("^[[:alpha:]][[:alnum:]+.-]*://", path) ||
    !file.exists(path) || dir.exists(path)) {
    cli::cli_abort("{.arg path} must identify an existing local CSV file.")
  }
  if (!is.null(source_url)) {
    validate_single_string(source_url, "source_url")
    if (!grepl("^https?://[^/?#[:space:]]+", source_url) ||
      grepl("[[:space:]]", source_url)) {
      cli::cli_abort("{.arg source_url} must be an HTTP(S) source URL.")
    }
  }
  file <- onet_local_source_snapshot(path, expected_sha256 = expected_sha256, as_of = as_of)
  on.exit(unlink(file, force = TRUE), add = TRUE)
  receipt <- attr(file, "source_receipt", exact = TRUE)
  if (!is.null(source_url)) {
    receipt <- onet_source_receipt(
      file, source_url = source_url, source_path = receipt$source_path,
      expected_sha256 = expected_sha256, as_of = as_of,
      retrieved_at = receipt$retrieved_at
    )
  }
  raw <- openai_iwa_parse_csv(file)
  if (any(!grepl("^[0-9]{4}-(01|04|07|10)-01$", raw$quarter))) {
    cli::cli_abort("OpenAI IWA {.var quarter} must be an ISO quarter-start date.")
  }
  if (any(!grepl("^[A-Z]{2}$", raw$country))) {
    cli::cli_abort("OpenAI IWA {.var country} must contain two-letter uppercase country codes.")
  }
  other <- raw$iwa_cleaned == "Other IWA"
  if (any(!other & !grepl("^4\\.A\\.[0-9]+\\.[a-z]\\.[0-9]+\\.I[0-9]{2}$", raw$iwa_cleaned))) {
    cli::cli_abort("OpenAI IWA categories must be IWA identifiers or {.val Other IWA}.")
  }
  valid_number <- grepl(
    "^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$",
    raw$share_of_messages
  )
  if (any(!valid_number)) {
    cli::cli_abort("OpenAI IWA shares must be nonmissing decimal numbers.")
  }
  shares <- as.numeric(raw$share_of_messages)
  if (any(!is.finite(shares) | shares < 0 | shares > 1)) {
    cli::cli_abort("OpenAI IWA shares must be finite and between zero and one.")
  }
  if (anyDuplicated(raw[c("quarter", "country", "iwa_cleaned")])) {
    cli::cli_abort("OpenAI IWA country-quarter-category keys must be unique.")
  }
  out <- tibble::tibble(
    quarter = raw$quarter, country_code = raw$country,
    iwa_category = raw$iwa_cleaned,
    iwa_id = replace(raw$iwa_cleaned, other, NA_character_),
    is_other_iwa = other, share_of_messages = shares,
    source_path = rep(receipt$source_path, nrow(raw)),
    source_url = rep(receipt$source_url %||% NA_character_, nrow(raw)),
    source_sha256 = rep(receipt$actual_sha256, nrow(raw)),
    source_read_at = rep(
      format(receipt$retrieved_at, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"), nrow(raw)
    ),
    source_as_of = rep(receipt$as_of %||% NA_character_, nrow(raw)),
    source_record = seq_len(nrow(raw))
  )
  attr(out, "source_receipt") <- receipt
  out
}

openai_iwa_parse_csv <- function(file) {
  tryCatch(
    withCallingHandlers({
      bytes <- readBin(file, "raw", n = file.info(file)$size)
      if (length(bytes) >= 3L &&
        identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
        bytes <- bytes[-(1:3)]
      }
      text <- rawToChar(bytes)
      if (!nzchar(text) || !validUTF8(text)) {
        cli::cli_abort("OpenAI IWA CSV must be nonempty UTF-8 text.")
      }
      Encoding(text) <- "UTF-8"
      text <- sub("\r?\n$", "", text)
      if (grepl("[\r\n]$", text)) {
        cli::cli_abort("OpenAI IWA CSV must not contain blank records.")
      }
      lines <- strsplit(text, "\r?\n", perl = TRUE)[[1]]
      field <- '(?:[^,"\r\n]*|"(?:[^"\r\n]|"")*")'
      record <- paste0("^", field, "(?:,", field, "){3}$")
      if (!length(lines) || any(!grepl(record, lines, perl = TRUE))) {
        cli::cli_abort("OpenAI IWA CSV records must have four correctly quoted single-line fields.")
      }
      connection <- textConnection(lines)
      on.exit(close(connection), add = TRUE)
      out <- utils::read.csv(
        connection, colClasses = "character", check.names = FALSE,
        na.strings = character(), strip.white = FALSE, fill = FALSE,
        comment.char = "", row.names = NULL, blank.lines.skip = FALSE
      )
      required <- c("quarter", "country", "iwa_cleaned", "share_of_messages")
      if (anyDuplicated(names(out)) || !setequal(names(out), required)) {
        cli::cli_abort("OpenAI IWA CSV must have exactly the columns {.var {required}}.")
      }
      out[required]
    }, warning = function(cnd) {
      cli::cli_abort("Malformed OpenAI IWA CSV: {conditionMessage(cnd)}", parent = cnd)
    }),
    error = function(cnd) {
      cli::cli_abort("Could not read OpenAI IWA CSV: {conditionMessage(cnd)}", parent = cnd)
    }
  )
}
