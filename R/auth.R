#' Get O*NET API Key
#'
#' Retrieves the O*NET API key from the `ONET_API_KEY` environment variable.
#'
#' @return A character string containing the API key.
#'
#' @details
#' To obtain an API key, register at <https://services.onetcenter.org/developer/>.
#' Set the key in your environment with:
#' ```
#' Sys.setenv(ONET_API_KEY = "your-api-key")
#' ```
#' Or add it to your `.Renviron` file for persistence.
#'
#' @export
#' @examples
#' \dontrun{
#' Sys.setenv(ONET_API_KEY = "your-api-key")
#' onet_api_key()
#' }
onet_api_key <- function() {
  key <- Sys.getenv("ONET_API_KEY", unset = "")
  if (key == "") {
    cli_abort(c(
      "O*NET API key not found.",
      "i" = "Set your API key with {.code Sys.setenv(ONET_API_KEY = \"your-key\")}",
      "i" = "Get a key at {.url https://services.onetcenter.org/developer/}"
    ))

  }
  key
}

#' Check if O*NET API Key is Set
#'
#' @return Logical indicating if the API key is available.
#' @keywords internal
has_onet_api_key <- function() {
  Sys.getenv("ONET_API_KEY", unset = "") != ""
}
