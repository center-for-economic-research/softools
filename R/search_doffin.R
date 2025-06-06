#' Retrieve tender notices from Doffin using the Doffin API
#'
#' @param ... Additional parameters to be passed to the API.
#'
#' @returns A response object containing the search results.
#' @export
#'
#' @examples
#' resp <- search_doffin()
search_doffin <- function(...){
  # Base-url for Doffin notices seach API
  doffin_search_base_url <- "https://betaapi.doffin.no/public/v2/search?"

  # Create a request object
  req <-
    httr2::request(doffin_search_base_url)

  # Add header for API Key to request object
  # You can get a key by registering at
  # https://dof-notices-dev-api.developer.azure-api.net/
  req <- req |>
    httr2::req_headers(
      `Ocp-Apim-Subscription-Key` = Sys.getenv("DOFFIN_API_KEY")
    )

  # Define parameters for the search
  # List of available parameters can be found on API website
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
}
