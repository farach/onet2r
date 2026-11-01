#' Read a Local Stanford TaskNet Inventory
#'
#' Reads a manually acquired TaskNet CSV without downloading or caching it.
#' Study identifiers and ratings remain separate from official O&#42;NET
#' archive identifiers and estimates. Real TaskNet data are not bundled.
#'
#' @param path Path to an existing local UTF-8 CSV file.
#' @param source_url Optional public HTTP(S) source URL, supplied by the caller.
#' @param as_of Optional caller-supplied snapshot label.
#' @param expected_sha256 Optional expected SHA-256 digest of the input bytes.
#'
#' @return A tibble with `study_task_id`, `inventory`, `region`, `title`,
#'   `onet_soc_code`, `task`, raw relationship columns, numeric task ratings,
#'   and character research-score fields. Additional source columns remain
#'   character. Known optional columns are present even when absent from the
#'   file. Provenance columns are `source_path`, `source_url`, `source_sha256`,
#'   `source_read_at`, `source_as_of`, and integer `source_record`. The full
#'   source receipt is also retained as an attribute.
#'
#' @details
#' Required columns are `id`, `inventory`, `region`, `occupation`, `soc`,
#' `statement`, `refines`, `refined_by`, `overlaps`, `relevance`, `importance`,
#' and `frequency`. Empty rating cells become `NA_real_`; nonempty invalid
#' numbers are errors. Relevance and share ratings use 0--1, importance 1--5,
#' frequency 1--7, and duration and training 1--9. Fractional summaries are
#' retained without interpreting categories as time shares or exposure.
#'
#' The reader validates all relationship endpoints against the complete file.
#' `refines` and `refined_by` must be reciprocal, as must `overlaps`.
#' Endpoints must share an occupation code and belong to opposite inventories.
#' TaskNet rows may be `new` or `overlap`; comparison rows may be `missing`
#' or `overlap`. These labels describe inventory comparisons, not time change.
#'
#' The source `id` is a study ID even on O&#42;NET comparison rows. No official
#' task ID, release, or Core/Supplemental status is inferred. Research scores
#' are preserved as character source fields, not endorsed or selected.
#'
#' The digest identifies exactly the bytes parsed. Source URL and `as_of`
#' are caller assertions, not independently verified metadata. Read time is
#' not download time. Provenance describes the source bytes, not later edits.
#' [onet_tasknet_relations()] requires the unmodified reader result; construct
#' links before filtering either table. Header-only files return typed empties.
#' Confirm the separate CSV's redistribution terms before sharing real rows.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "tasknet-synthetic.csv", package = "onet2r")
#' tasks <- onet_tasknet_read(path, as_of = "synthetic example")
#' tasks[c("study_task_id", "inventory", "importance")]
#' onet_tasknet_relations(tasks)
onet_tasknet_read <- function(
    path,
    source_url = NULL,
    as_of = NULL,
    expected_sha256 = NULL) {
  validate_single_string(path, "path")
  if (!nzchar(trimws(path)) || grepl("^[[:alpha:]][[:alnum:]+.-]*://", path)) {
    cli::cli_abort("{.arg path} must be an existing local CSV file, not a URL.")
  }
  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort("{.arg path} must identify an existing regular file.")
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
  bytes <- tryCatch(
    readBin(file, "raw", n = file.info(file)$size),
    error = function(cnd) cli::cli_abort("Could not read the local TaskNet file.", parent = cnd)
  )
  raw <- tasknet_parse_csv(bytes)
  schema <- tasknet_schema()
  missing <- setdiff(schema$required, names(raw))
  if (length(missing)) {
    cli::cli_abort("TaskNet CSV is missing required columns: {.var {missing}}.")
  }
  reserved <- c(
    "study_task_id", "onet_soc_code", "title", "task",
    "source_path", "source_url", "source_sha256", "source_read_at", "source_as_of",
    "source_record"
  )
  if (any(names(raw) %in% reserved)) {
    cli::cli_abort("TaskNet CSV contains reserved output column names.")
  }
  for (column in setdiff(schema$columns, names(raw))) {
    raw[[column]] <- rep(NA_character_, nrow(raw))
  }
  raw <- raw[c(schema$columns, setdiff(names(raw), schema$columns))]
  names(raw)[match(c("id", "occupation", "soc", "statement"), names(raw))] <-
    c("study_task_id", "title", "onet_soc_code", "task")
  out <- tibble::as_tibble(raw)
  for (column in c(
    "study_task_id", "inventory", "region", "title", "onet_soc_code"
  )) {
    out[[column]] <- trimws(out[[column]])
  }
  for (column in names(schema$ratings)) {
    out[[column]] <- tasknet_parse_rating(
      out[[column]], column, out$study_task_id, schema$ratings[[column]]
    )
  }
  out$source_path <- rep(receipt$source_path, nrow(out))
  out$source_url <- rep(receipt$source_url %||% NA_character_, nrow(out))
  out$source_sha256 <- rep(receipt$actual_sha256, nrow(out))
  out$source_read_at <- rep(
    format(receipt$retrieved_at, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"), nrow(out)
  )
  out$source_as_of <- rep(receipt$as_of %||% NA_character_, nrow(out))
  out$source_record <- seq_len(nrow(out))
  tasknet_validate_rows(out)
  tasknet_build_relations(out)
  attr(out, "source_receipt") <- receipt
  attr(out, "tasknet_signature") <- tasknet_signature(out)
  out
}

#' Expand Explicit TaskNet Inventory Relationships
#'
#' Converts declared refinements and overlaps into unique TaskNet-to-comparison
#' edges. Reciprocal declarations are counted once. No matching, transitive
#' closure, weighting, or exposure estimation is performed.
#'
#' @param tasks The unmodified tibble returned by [onet_tasknet_read()].
#'
#' @return A tibble with `from_study_task_id`, `to_study_task_id`,
#'   `relation_type` (`"refines"` or `"overlaps"`), `onet_soc_code`,
#'   `from_inventory`, `to_inventory`, and the reader's provenance columns.
#'   The `from` endpoint is always TaskNet. Refinement means that this
#'   statement refines the comparison statement at `to`. Empty inputs and
#'   relation-free inventories have the same typed output schema. Distinct
#'   relationship types on the same pair are retained as separate edges, so
#'   edge counts need not equal counts of distinct statement pairs.
#'
#' @details
#' Build relationships before filtering or editing the reader output. A content
#' signature rejects modified data rather than presenting derived declarations
#' as relationships from the original source bytes. This is an integrity guard,
#' not a security or authenticity signature.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "tasknet-synthetic.csv", package = "onet2r")
#' onet_tasknet_relations(onet_tasknet_read(path))
onet_tasknet_relations <- function(tasks) {
  if (!is.data.frame(tasks) ||
    !identical(attr(tasks, "tasknet_signature", exact = TRUE), tasknet_signature(tasks))) {
    cli::cli_abort(
      "{.arg tasks} must be the unchanged reader result; build relations before filtering or modifying it."
    )
  }
  tasknet_validate_rows(tasks)
  tasknet_build_relations(tasks)
}

tasknet_schema <- function() {
  ratings <- list(
    relevance = c(0, 1), importance = c(1, 5), frequency = c(1, 7),
    duration = c(1, 9), training_svp = c(1, 9),
    articulability = c(0, 1), verifiability = c(0, 1), top_worker_gap = c(0, 1)
  )
  list(
    required = c(
      "id", "inventory", "region", "occupation", "soc", "statement",
      "refines", "refined_by", "overlaps", "relevance", "importance", "frequency"
    ),
    columns = c(
      "id", "inventory", "region", "occupation", "soc", "statement",
      "refines", "refined_by", "overlaps", names(ratings), "eloundou_beta",
      "rl_feasibility", "chat_use_quartile", "postings_quartile", "themes"
    ),
    ratings = ratings
  )
}

tasknet_parse_csv <- function(bytes) {
  if (length(bytes) >= 3L &&
    identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    bytes <- bytes[-(1:3)]
  }
  tryCatch(
    withCallingHandlers({
      text <- rawToChar(bytes)
      if (!nzchar(text) || !validUTF8(text)) {
        cli::cli_abort("TaskNet CSV must be nonempty UTF-8 text with a header.")
      }
      Encoding(text) <- "UTF-8"
      tasknet_validate_quotes(text)
      text <- sub("\r?\n$", "", text)
      fields_connection <- textConnection(text)
      on.exit(close(fields_connection), add = TRUE)
      fields <- utils::count.fields(
        fields_connection, sep = ",", quote = "\"", comment.char = "",
        blank.lines.skip = FALSE
      )
      if (!length(fields) || is.na(fields[1]) ||
        any(fields[!is.na(fields)] != fields[1])) {
        cli::cli_abort("TaskNet CSV has malformed rows or inconsistent field counts.")
      }
      connection <- textConnection(text)
      on.exit(close(connection), add = TRUE)
      out <- utils::read.csv(
        connection, colClasses = "character", check.names = FALSE,
        na.strings = character(), strip.white = FALSE, fill = FALSE,
        comment.char = "", row.names = NULL, blank.lines.skip = FALSE
      )
      if (any(!nzchar(trimws(names(out)))) || anyDuplicated(names(out))) {
        cli::cli_abort("TaskNet CSV column names must be nonblank and unique.")
      }
      out
    }, warning = function(cnd) {
      cli::cli_abort("Malformed TaskNet CSV: {conditionMessage(cnd)}", parent = cnd)
    }),
    error = function(cnd) {
      cli::cli_abort("Could not parse TaskNet CSV: {conditionMessage(cnd)}", parent = cnd)
    }
  )
}

tasknet_validate_quotes <- function(text) {
  state <- "start"
  for (token in strsplit(text, "", fixed = TRUE)[[1]]) {
    if (state == "quoted") {
      if (token == "\"") {
        state <- "closed"
      }
    } else if (token == "\"") {
      if (!state %in% c("start", "closed")) {
        cli::cli_abort("Invalid TaskNet CSV quoting: a quote occurs inside an unquoted field.")
      }
      state <- "quoted"
    } else if (token %in% c(",", "\r", "\n")) {
      state <- "start"
    } else {
      if (state == "closed") {
        cli::cli_abort("Invalid TaskNet CSV quoting: text follows a closing quote.")
      }
      state <- "unquoted"
    }
  }
  if (state == "quoted") {
    cli::cli_abort("Invalid TaskNet CSV quoting: an opening quote is not closed.")
  }
  invisible(NULL)
}

tasknet_parse_rating <- function(x, column, ids, bounds) {
  text <- trimws(x)
  missing <- is.na(text) | !nzchar(text)
  number <- rep(NA_real_, length(text))
  valid_token <- grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", text)
  bad <- !missing & !valid_token
  if (any(bad)) {
    cli::cli_abort("Invalid {.var {column}} number for study ID {.val {ids[which(bad)[1]]}}.")
  }
  number[!missing] <- as.numeric(text[!missing])
  bad <- !missing & (!is.finite(number) | number < bounds[1] | number > bounds[2])
  if (any(bad)) {
    cli::cli_abort("Out-of-range {.var {column}} rating for study ID {.val {ids[which(bad)[1]]}}.")
  }
  number
}

tasknet_validate_rows <- function(data) {
  for (column in c("study_task_id", "inventory", "region", "title", "onet_soc_code", "task")) {
    if (any(is.na(data[[column]]) | !nzchar(trimws(data[[column]])))) {
      cli::cli_abort("TaskNet {.var {column}} must not contain missing or blank values.")
    }
  }
  if (anyDuplicated(data$study_task_id)) {
    cli::cli_abort("TaskNet study IDs must be unique.")
  }
  if (any(grepl("[;[:space:]]", data$study_task_id))) {
    cli::cli_abort("TaskNet study IDs must not contain whitespace or semicolons.")
  }
  if (any(!grepl("^[0-9]{2}-[0-9]{4}\\.[0-9]{2}$", data$onet_soc_code))) {
    cli::cli_abort("TaskNet {.var onet_soc_code} must contain native O*NET-SOC detail codes.")
  }
  if (any(!data$inventory %in% c("TaskNet", "O*NET"))) {
    cli::cli_abort("TaskNet {.var inventory} must be {.val TaskNet} or {.val O*NET}.")
  }
  valid_region <- (data$inventory == "TaskNet" & data$region %in% c("new", "overlap")) |
    (data$inventory == "O*NET" & data$region %in% c("missing", "overlap"))
  if (any(!valid_region)) {
    cli::cli_abort("TaskNet {.var region} is incompatible with its inventory.")
  }
  invisible(data)
}

tasknet_signature <- function(data) {
  digest::digest(
    list(columns = lapply(data, identity), receipt = attr(data, "source_receipt", exact = TRUE)),
    algo = "sha256"
  )
}

tasknet_build_relations <- function(data) {
  declarations <- list()
  index <- 0L
  for (column in c("refines", "refined_by", "overlaps")) {
    for (row in seq_len(nrow(data))) {
      field <- data[[column]][row]
      if (is.na(field) || !nzchar(trimws(field))) {
        next
      }
      if (grepl("(^|;)[[:space:]]*(;|$)", field)) {
        cli::cli_abort("Empty {.var {column}} target for study ID {.val {data$study_task_id[row]}}.")
      }
      targets <- trimws(strsplit(field, ";", fixed = TRUE)[[1]])
      if (anyDuplicated(targets)) {
        cli::cli_abort("Duplicate {.var {column}} target for study ID {.val {data$study_task_id[row]}}.")
      }
      target_rows <- match(targets, data$study_task_id)
      if (anyNA(target_rows)) {
        cli::cli_abort("Unresolved {.var {column}} target for study ID {.val {data$study_task_id[row]}}.")
      }
      if (any(target_rows == row) ||
        any(data$onet_soc_code[target_rows] != data$onet_soc_code[row]) ||
        any(data$inventory[target_rows] == data$inventory[row])) {
        cli::cli_abort("Invalid {.var {column}} endpoint for study ID {.val {data$study_task_id[row]}}.")
      }
      if ((column == "refines" && data$inventory[row] != "TaskNet") ||
        (column == "refined_by" && data$inventory[row] != "O*NET")) {
        cli::cli_abort("Wrong {.var {column}} direction for study ID {.val {data$study_task_id[row]}}.")
      }
      from <- if (data$inventory[row] == "TaskNet") rep(row, length(target_rows)) else target_rows
      to <- if (data$inventory[row] == "TaskNet") target_rows else rep(row, length(target_rows))
      index <- index + 1L
      declarations[[index]] <- tibble::tibble(
        from = from, to = to,
        relation_type = if (column == "overlaps") "overlaps" else "refines",
        declared_by = rep(data$inventory[row], length(target_rows))
      )
    }
  }
  edges <- if (length(declarations)) {
    purrr::list_rbind(declarations)
  } else {
    tibble::tibble(from = integer(), to = integer(), relation_type = character(), declared_by = character())
  }
  counts <- edges |>
    dplyr::count(.data$from, .data$to, .data$relation_type, name = "declarations")
  if (any(counts$declarations != 2L)) {
    cli::cli_abort("TaskNet relation declarations must be reciprocal across both inventories.")
  }
  linked <- unique(c(edges$from, edges$to))
  if (any((data$region == "overlap") != (seq_len(nrow(data)) %in% linked))) {
    cli::cli_abort("TaskNet {.var region} must agree with the declared relationship endpoints.")
  }
  edges <- edges |>
    dplyr::distinct(.data$from, .data$to, .data$relation_type) |>
    dplyr::arrange(.data$from, .data$to, .data$relation_type)
  out <- tibble::tibble(
    from_study_task_id = data$study_task_id[edges$from],
    to_study_task_id = data$study_task_id[edges$to],
    relation_type = edges$relation_type,
    onet_soc_code = data$onet_soc_code[edges$from],
    from_inventory = data$inventory[edges$from],
    to_inventory = data$inventory[edges$to],
    from_source_record = data$source_record[edges$from],
    to_source_record = data$source_record[edges$to]
  )
  for (column in c("source_path", "source_url", "source_sha256", "source_read_at", "source_as_of")) {
    out[[column]] <- data[[column]][edges$from]
  }
  out
}
