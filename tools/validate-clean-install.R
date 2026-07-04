# Clean external validation for onet2r.
#
# This script installs the package into temporary libraries, switches to
# temporary working directories outside the repository, and runs the installed
# validation script without devtools::load_all().
#
# Usage:
#   Rscript tools\validate-clean-install.R
#   Rscript tools\validate-clean-install.R --rounds=3
#   Rscript tools\validate-clean-install.R --live-api --live-archives

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(prefix, default = NULL) {
  match <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(match) == 0) {
    return(default)
  }
  sub(paste0("^", prefix, "="), "", match[[1]])
}

has_flag <- function(flag) {
  flag %in% args
}

rounds <- as.integer(arg_value("--rounds", "2"))
if (is.na(rounds) || rounds < 1) {
  stop("--rounds must be a positive integer.", call. = FALSE)
}

repo <- normalizePath(arg_value("--repo", getwd()), winslash = "\\", mustWork = TRUE)
description <- file.path(repo, "DESCRIPTION")
if (!file.exists(description)) {
  stop("Could not find DESCRIPTION under --repo: ", repo, call. = FALSE)
}
description_lines <- readLines(description, warn = FALSE)
if (!any(grepl("^Package:\\s+onet2r\\s*$", description_lines))) {
  stop("--repo does not look like the onet2r package root: ", repo, call. = FALSE)
}

r_bin <- file.path(R.home("bin"), "R")
rscript_bin <- file.path(R.home("bin"), "Rscript")
live_api <- has_flag("--live-api")
live_archives <- has_flag("--live-archives")

run_checked <- function(command, args, env = character(), cwd = NULL) {
  old_wd <- NULL
  if (!is.null(cwd)) {
    old_wd <- getwd()
    setwd(cwd)
    on.exit(setwd(old_wd), add = TRUE)
  }
  status <- system2(command, args = args, env = env)
  if (!identical(status, 0L)) {
    stop("Command failed with status ", status, ": ", command, call. = FALSE)
  }
  invisible(TRUE)
}

write_validation_driver <- function(path, lib, live_api, live_archives) {
  lines <- c(
    "options(cli.num_colors = 1, crayon.enabled = FALSE, pillar.bold = FALSE)",
    sprintf(".libPaths(c(%s, .libPaths()))", deparse(lib)),
    "setwd(tempdir())",
    if (live_api) {
      "message('ONET_API_KEY is available: ', nzchar(Sys.getenv('ONET_API_KEY')))"
    } else {
      "Sys.unsetenv('ONET_API_KEY')"
    },
    "validation <- system.file('examples', 'validate-outputs.R', package = 'onet2r')",
    "if (!nzchar(validation)) stop('Installed validation script was not found.', call. = FALSE)",
    if (live_archives) {
      "Sys.setenv(ONET2R_VALIDATE_LIVE = 'true')"
    } else {
      "Sys.unsetenv('ONET2R_VALIDATE_LIVE')"
    },
    "source(validation, echo = FALSE)",
    "message('Clean validation completed from: ', getwd())"
  )
  writeLines(lines, path, useBytes = TRUE)
}

message("Repository: ", repo)
message("Rounds: ", rounds)
message("Live O*NET API checks: ", if (live_api) "enabled" else "disabled")
message("Live archive/release checks: ", if (live_archives) "enabled" else "disabled")

for (round in seq_len(rounds)) {
  lib <- tempfile(sprintf("onet2r-clean-lib-%02d-", round))
  workdir <- tempfile(sprintf("onet2r-clean-work-%02d-", round))
  dir.create(lib, recursive = TRUE)
  dir.create(workdir, recursive = TRUE)

  message("")
  message("Round ", round, ": installing into ", lib)
  run_checked(
    r_bin,
    c("CMD", "INSTALL", paste0("--library=", shQuote(lib)), shQuote(repo)),
    cwd = workdir
  )

  driver <- file.path(workdir, "run-validation.R")
  write_validation_driver(
    driver,
    normalizePath(lib, winslash = "\\", mustWork = TRUE),
    live_api = live_api,
    live_archives = live_archives
  )

  message("Round ", round, ": running installed-package validation from ", workdir)
  run_checked(
    rscript_bin,
    c("--vanilla", shQuote(driver)),
    cwd = workdir
  )
}

message("")
message("All clean external validation rounds passed.")
