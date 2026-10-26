# Rscript data-raw/occupational-cross-domain.R path/to/db_30_3_text.zip
library(dplyr)
options(cli.progress_show_after = Inf)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[[1]])) {
  stop("Supply one existing local db_30_3_text.zip.", call. = FALSE)
}
archive <- args[[1]]
expected <- "7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780"
stopifnot(identical(digest::digest(file = archive, algo = "sha256"), expected))
for (file in c("occupational-distinctiveness.R", "software-distinctiveness.R",
               "occupational-neighbors.R")) {
  source(file.path("inst", "article-helpers", file))
}
out <- file.path("inst", "extdata", "occupational-extensions")
read_snapshot <- function(name) {
  tibble::as_tibble(utils::read.csv(
    snapshot_csv_path(file.path(out, paste0(name, ".csv")))
  ))
}
save_table <- function(data, name) {
  suffix <- if (name %in% c("cross-domain-neighbors", "cross-domain-neighbor-overlap")) {
    ".csv.gz"
  } else {
    ".csv"
  }
  write_snapshot_csv(data, file.path(out, paste0(name, suffix)))
}
roster <- read_snapshot("skills-common-universe")
universe <- sort(roster$onet_soc_code[roster$in_common_universe])
stopifnot(length(universe) > 1L, !anyNA(universe), !anyDuplicated(universe))
entries <- utils::unzip(archive, list = TRUE)$Name
read_raw <- function(name) {
  entry <- entries[basename(entries) == paste0(name, ".txt")]
  stopifnot(length(entry) == 1L)
  connection <- unz(archive, entry, open = "r")
  on.exit(close(connection), add = TRUE)
  tibble::as_tibble(utils::read.delim(
    connection, quote = "", check.names = FALSE, stringsAsFactors = FALSE
  ))
}
activities <- read_raw("Tasks to DWAs") |>
  transmute(occupation = .data[["O*NET-SOC Code"]],
            item = .data[["DWA Element ID"]]) |>
  distinct() |>
  filter(.data$occupation %in% universe) |>
  mutate(weight = 1)
software <- read_raw("Software Skills") |>
  transmute(occupation = .data[["O*NET-SOC Code"]],
            item = .data[["Workplace Example"]]) |>
  distinct() |>
  filter(.data$occupation %in% universe) |>
  mutate(weight = 1)
occupation_names <- read_raw("Occupation Data") |>
  transmute(occupation = .data[["O*NET-SOC Code"]], title = .data$Title)
links <- list(dwa = activities, software = software)
binary_neighbors <- bind_rows(lapply(names(links), function(block) {
  binary_profile_pairs(links[[block]], universe) |>
    mutate(distance = round(.data$distance, 8L)) |>
    profile_neighbors(10L) |>
    rename(rank = "neighbor_rank") |>
    mutate(block = block)
}))
skill_neighbors <- read_snapshot("skills-nearest-neighbors")
neighbors <- bind_rows(binary_neighbors, skill_neighbors) |>
  select(block, occupation, neighbor, distance, rank) |>
  arrange(.data$block, .data$occupation, .data$rank, .data$neighbor)
blocks <- c("dwa", "software", "es_im", "es_lv", "ts_im", "ts_lv")
stopifnot(setequal(neighbors$block, blocks))
for (block in blocks) {
  data <- neighbors[neighbors$block == block, ]
  stopifnot(setequal(data$occupation, universe),
            all(data$neighbor %in% universe),
            all(data$occupation != data$neighbor))
}
save_table(neighbors, "cross-domain-neighbors")
overlap <- bind_rows(lapply(utils::combn(blocks, 2, simplify = FALSE), function(pair) {
  a <- filter(neighbors, .data$block == pair[[1]])
  b <- filter(neighbors, .data$block == pair[[2]])
  bind_rows(lapply(c(1L, 5L, 10L), function(k) {
    neighbor_set_overlap(a, b, k) |>
      mutate(block_a = pair[[1]], block_b = pair[[2]], k = k)
  }))
}))
save_table(overlap, "cross-domain-neighbor-overlap")
save_table(overlap |>
             summarise(
               n_occupations = n(), median_overlap = median(jaccard),
               mean_overlap = mean(jaccard),
               no_shared_neighbor_share = mean(n_both == 0L),
               .by = c("block_a", "block_b", "k")
             ), "cross-domain-overlap-summary")
selected <- c("13-2011.00", "15-1252.00", "27-1021.00", "29-1141.00")
excluded_selected <- setdiff(selected, universe)
if (length(excluded_selected)) {
  cli::cli_inform("Selected codes outside primary support: {.val {excluded_selected}}.")
}
selected <- intersect(selected, universe)
stopifnot(length(selected) > 0L)
save_table(neighbors |>
             filter(.data$occupation %in% selected, .data$rank == 1L) |>
             left_join(occupation_names, by = "occupation",
                       relationship = "many-to-one") |>
             rename(occupation_title = "title") |>
             left_join(occupation_names, by = c("neighbor" = "occupation"),
                       relationship = "many-to-one") |>
             rename(neighbor_title = "title"),
           "cross-domain-selected-neighbors")
save_table(tibble::tibble(
  release = "30.3", source_sha256 = expected, n_occupations = length(universe),
  eligibility = "Complete four skill blocks with no recommended-suppressed rating",
  binary_distance = "Total variation on exact-name or DWA presence",
  skill_distance = "Mean absolute rating gap divided by scale range",
  distance_digits = 8L,
  comparison = "Neighbor-set Jaccard at k = 1, 5, 10; cutoff ties retained"
), "cross-domain-provenance")
print(filter(read_snapshot("cross-domain-overlap-summary"),
             .data$block_a == "dwa" | .data$block_a == "software"))
