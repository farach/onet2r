# =============================================================================
# Census/PUMS helpers
# =============================================================================

#' Create SOC Employment Weights from PUMS Microdata
#'
#' Converts American Community Survey (ACS), Current Population Survey (CPS), or
#' similar microdata with occupation codes into SOC-level employment weights.
#' Use this after downloading PUMS data with packages such as `tidycensus` or
#' `ipumsr`.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param pums A data frame containing individual-level occupation records.
#' @param socp Name of the occupation-code column. ACS PUMS commonly uses
#'   `"SOCP"`.
#' @param weight Optional name of the person-weight column. If `NULL`, each row
#'   receives weight 1. ACS PUMS commonly uses `"PWGTP"`.
#'
#' @return A tibble with `soc_code`, `employment`, and `records`.
#'
#' @section Lifecycle:
#' This helper is soft-deprecated in favor of [onet_weight_panel_pums()], which
#' keeps source and reference taxonomies explicit.
#' @export
#'
#' @examples
#' pums <- tibble::tibble(
#'   SOCP = c("151252", "151252", "291141"),
#'   PWGTP = c(120, 80, 200)
#' )
#'
#' suppressWarnings(onet_pums_employment_weights(pums))
onet_pums_employment_weights <- function(
    pums,
    socp = "SOCP",
    weight = "PWGTP") {
  lifecycle::deprecate_soft(
    "0.4.0",
    "onet_pums_employment_weights()",
    "onet_weight_panel_pums()",
    details = "Use `onet_weight_panel_pums()` for vintage-aware PUMS weights."
  )
  if (!is.data.frame(pums)) {
    cli::cli_abort("{.arg pums} must be a data frame.")
  }
  validate_single_column(pums, socp, "socp")
  if (!is.null(weight)) {
    validate_single_column(pums, weight, "weight")
  }

  pums <- tibble::as_tibble(pums)
  pums$.onet_soc_code <- standardize_soc_code(pums[[socp]])
  pums$.onet_weight <- if (is.null(weight)) {
    1
  } else {
    parse_oews_number(pums[[weight]])
  }

  out <- pums |>
    dplyr::filter(!is.na(.data$.onet_soc_code), .data$.onet_soc_code != "") |>
    dplyr::summarise(
      employment = sum(.data$.onet_weight, na.rm = TRUE),
      records = dplyr::n(),
      .by = ".onet_soc_code"
    )

  names(out)[names(out) == ".onet_soc_code"] <- "soc_code"

  out |>
    dplyr::arrange(dplyr::desc(.data$employment))
}
