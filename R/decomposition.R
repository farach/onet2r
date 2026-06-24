# =============================================================================
# Within-between decomposition
# =============================================================================

#' Decompose Aggregate Change into Within and Between Components
#'
#' Decomposes aggregate change between two periods into within-occupation,
#' between-occupation, interaction, and unclassifiable components. The within
#' term is restricted to rows marked comparable.
#'
#' @param from_scores Initial-period occupation scores.
#' @param to_scores Final-period occupation scores.
#' @param from_weights Initial-period weights.
#' @param to_weights Final-period weights.
#' @param occupation_code Join column for scores and weights.
#' @param score Score column in score tables.
#' @param weight Weight column in weight tables.
#' @param comparable Optional logical column in `to_scores` or `from_scores`
#'   marking rows safe for within-change attribution.
#'
#' @return A tibble with decomposition components. Coverage information is
#'   attached as a `coverage` attribute.
#' @export
onet_decompose_change <- function(
    from_scores,
    to_scores,
    from_weights,
    to_weights,
    occupation_code = "reference_soc_code",
    score = "measure_score",
    weight = "employment",
    comparable = "safely_comparable") {
  inputs <- list(from_scores, to_scores, from_weights, to_weights)
  if (!all(vapply(inputs, is.data.frame, logical(1)))) {
    cli::cli_abort("Score and weight inputs must be data frames.")
  }
  validate_single_column(from_scores, occupation_code, "occupation_code")
  validate_single_column(to_scores, occupation_code, "occupation_code")
  validate_single_column(from_scores, score, "score")
  validate_single_column(to_scores, score, "score")
  validate_single_column(from_weights, occupation_code, "occupation_code")
  validate_single_column(to_weights, occupation_code, "occupation_code")
  validate_single_column(from_weights, weight, "weight")
  validate_single_column(to_weights, weight, "weight")

  scores <- dplyr::inner_join(
    prep_score_period(from_scores, occupation_code, score, "from", comparable),
    prep_score_period(to_scores, occupation_code, score, "to", comparable),
    by = occupation_code,
    relationship = "one-to-one"
  )
  weights <- dplyr::inner_join(
    prep_weight_period(from_weights, occupation_code, weight, "from"),
    prep_weight_period(to_weights, occupation_code, weight, "to"),
    by = occupation_code,
    relationship = "one-to-one"
  )
  data <- dplyr::inner_join(scores, weights, by = occupation_code, relationship = "one-to-one")
  if (nrow(data) == 0) {
    cli::cli_abort("No common occupations were available for decomposition.")
  }

  data$w_from <- data$weight_from / sum(data$weight_from, na.rm = TRUE)
  data$w_to <- data$weight_to / sum(data$weight_to, na.rm = TRUE)
  data$score_change <- data$score_to - data$score_from
  data$weight_change <- data$w_to - data$w_from
  data$comparable <- data$comparable_to %||% data$comparable_from
  data$comparable[is.na(data$comparable)] <- TRUE

  within_all <- sum(data$w_from * data$score_change, na.rm = TRUE)
  within <- sum(data$w_from[data$comparable] * data$score_change[data$comparable], na.rm = TRUE)
  unclassifiable <- within_all - within
  between <- sum(data$weight_change * data$score_from, na.rm = TRUE)
  interaction <- sum(data$weight_change * data$score_change, na.rm = TRUE)
  total_change <- sum(data$w_to * data$score_to, na.rm = TRUE) -
    sum(data$w_from * data$score_from, na.rm = TRUE)

  result <- tibble::tibble(
    component = c("within", "between", "interaction", "unclassifiable", "total_change"),
    value = c(within, between, interaction, unclassifiable, total_change)
  )
  attr(result, "coverage") <- tibble::tibble(
    n_common = nrow(data),
    n_safely_comparable = sum(data$comparable, na.rm = TRUE),
    leakage = total_change - sum(result$value[result$component != "total_change"], na.rm = TRUE)
  )
  result
}

prep_score_period <- function(data, occupation_code, score, period, comparable) {
  out <- tibble::tibble(
    reference_soc_code = as.character(data[[occupation_code]]),
    score_value = parse_onet_number(data[[score]])
  )
  names(out) <- c(occupation_code, paste0("score_", period))
  comparable_name <- paste0("comparable_", period)
  if (!is.null(comparable) && comparable %in% names(data)) {
    out[[comparable_name]] <- as.logical(data[[comparable]])
  } else {
    out[[comparable_name]] <- TRUE
  }
  out
}

prep_weight_period <- function(data, occupation_code, weight, period) {
  out <- tibble::tibble(
    reference_soc_code = as.character(data[[occupation_code]]),
    weight_value = parse_onet_number(data[[weight]])
  )
  names(out) <- c(occupation_code, paste0("weight_", period))
  out
}
