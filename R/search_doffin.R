#' Check if running under testthat
#' @keywords internal
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

## Set the Doffin API key in the environment
#'
#' Optionally prompts the user for the key if not supplied and askpass is available.
#' @param key The API key to set. If NULL, will prompt the user (if askpass is installed).
#' @export
set_doffin_api_key <- function(key = NULL) {
  if (is.null(key)) {
    if (requireNamespace("askpass", quietly = TRUE)) {
      key <- askpass::askpass("Please enter your Doffin API key")
    } else {
      stop("Please supply a key or install the 'askpass' package for interactive entry.")
    }
  }
  Sys.setenv("DOFFIN_API_KEY" = key)
  invisible(key)
}

#' Retrieve the Doffin API key
#'
#' This function retrieves the Doffin API key from the `api_key` argument or the DOFFIN_API_KEY environment variable.
#' @param api_key Optionally, supply the API key directly. If NULL, will look for the DOFFIN_API_KEY environment variable.
#' @return The API key as a string.
#' @keywords internal
get_doffin_api_key <- function(api_key = NULL) {
  key <- if (!is.null(api_key)) api_key else Sys.getenv("DOFFIN_API_KEY")
  if (!identical(key, "")) {
    return(key)
  }
  if (is_testing()) {
    stop("No API key found for testing. Please set DOFFIN_API_KEY in your test environment.")
  }
  stop("No API key found. Please supply it with the `api_key` argument or set the DOFFIN_API_KEY environment variable.")
}

##' Retrieve tender notices from Doffin using the Doffin API.
##' @section API Key Setup:
##' This function requires a Doffin API key. Set it using one of these methods:
##' \itemize{
##'   \item Store in `.Renviron`: `DOFFIN_API_KEY=your-key` (recommended)
##'   \item Interactive setup: `set_doffin_api_key()`
##'   \item Direct argument: `search_doffin(api_key = "your-key")`
##' }
##'
##' A key can be obtained by registering at the Doffin API portal: \url{https://dof-notices-dev-api.developer.azure-api.net/}.
#' @param ... Additional parameters to be passed to the API.
#' @param api_key Optionally, supply the API key directly. If NULL, will look for the DOFFIN_API_KEY environment variable.
#'
#' @returns A response object containing the search results.
#' @export
#'
#' @examples
#' \dontrun{
#' if (Sys.getenv("DOFFIN_API_KEY") != "") {
#'   resp <- search_doffin()
#' }
#' }
search_doffin <- function(..., api_key = NULL) {
  # Base-url for Doffin notices search API
  doffin_search_base_url <- "https://betaapi.doffin.no/public/v2/search?"

  # Get API key
  key <- get_doffin_api_key(api_key)

  # Create a request object
  req <- httr2::request(doffin_search_base_url)

  # Add header for API Key to request object
  req <- req |>
    httr2::req_headers(
      `Ocp-Apim-Subscription-Key` = key
    )

  # Define parameters for the search
  req <- req |>
    httr2::req_url_query(
      type = "COMPETITION",
      ...
    )

  # Send request to API
  resp <- httr2::req_perform(req)

  # Check if the request was successful
  if (httr2::resp_status(resp) != 200) {
    stop("Failed to fetch data from Doffin API: ", httr2::resp_status(resp))
  }

  # Check response content type
  if (httr2::resp_content_type(resp) != "application/json") {
    stop("Unexpected content type: ", httr2::resp_content_type(resp))
  }

  return(resp)
}
