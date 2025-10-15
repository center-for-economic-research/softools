#' Get municipality codes and validity dates for a given period
#'
#' This function retrieves municipality codes and their validity periods from
#' Statistics Norway's classification system for a specified time range. If no
#' years are provided, it defaults to the current year.
#'
#' @param start_year Numeric. First year to include (default: current year).
#'   Must be a four-digit year.
#' @param end_year Numeric. Last year to include (default: same as start_year).
#'   Must be a four-digit year and greater than or equal to start_year.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{code}{Municipality code, four digit number}
#'     \item{validFromInRequestedRange}{Start date of validity period withing specified time range}
#'     \item{validToInRequestedRange}{End date of validity within specified time range. Excludes the date itself, meaning that the code is not necessarily valid on the returned date}
#'     \item{name}{Last valid name associated with ID}
#'   }
#'
#' @details
#' The function uses Statistics Norway's classification code 131 to retrieve municipality
#' structure. It focuses on municipality codes rather than names, as codes are
#' the primary identifier. Name changes (e.g., addition of Sami names) do not
#' affect the municipality code.
#'
#' @examples
#' \dontrun{
#' # Get municipality structure for current year
#' get_municipality_structure()
#'
#' # Get structure for a specific year
#' get_municipality_structure(2020)
#'
#' # Get structure for a period
#' # Note: Trondheim 1601 will have valid to date of 2018-01-01,
#' # but the code ceased to exist on this date 2018
#' get_municipality_structure(2017, 2022)
#'
#' }
#'
#' @export
#' @importFrom klassR get_klass
#' @importFrom dplyr group_by summarise arrange filter
#' @importFrom rlang .data
get_municipality_structure <- function(
    start_year = as.numeric(format(Sys.Date(), "%Y")),
    end_year = start_year) {

  # Input validation
  if (!is.numeric(start_year) || !is.numeric(end_year)) {
    stop("Both start_year and end_year must be numeric.")
  }

  if (length(start_year) != 1 || length(end_year) != 1) {
    stop("start_year and end_year must be single values.")
  }

  if (start_year != round(start_year) || end_year != round(end_year)) {
    stop("start_year and end_year must be integers.")
  }

  if (nchar(as.character(start_year)) != 4 || nchar(as.character(end_year)) != 4) {
    stop("Both start_year and end_year must be four-digit years.")
  }

  if (start_year > end_year) {
    stop("start_year must be less than or equal to end_year.")
  }

  # Set dates - municipality structure is always valid from January 1st of the year
  start_date <- paste0(start_year, "-01-01")
  # Until the end of the year
  # Note that get_klass will return a valid to date which is one day later than
  # the specified end date.
  end_date <- paste0(end_year, "-12-31")

  periode <- c(start_date, end_date)

  # Get classification of municipalities for the given period
  klass_muni <- tryCatch(
    klassR::get_klass(
      # Classification number for municipalities
      131,
      date = periode
    ),
    error = function(e) {
      stop("Error fetching municipality structure from klassR: ", e$message)
    }
  )

  # Fjern kode for uoppgitt kommune
  klass_muni <- klass_muni |>
    dplyr::filter(.data$code != "9999")

  # Validity dates returned from get_klass also cover names
  # We are only interested in codes because they identify the municipality
  # Changing codes signify change in unit
  # Names can change without municipality changing
  # For example, Tjeldsund (1852) got an additional Sami name in 2018
  # but the code did not change
  # SSB also does not distinguish between name changes
  # Municipalities are treated as the same based on code
  # Retrieving data on population from Tjeldsund (1852) from table 06913
  # for the period 2001-2019 will return data for Tjeldsund (1909-2019)

  # Get code and validity date
  # Find the first and last date for each code
  # I do this to get one code by municipality, disregarding name changes
  klass_muni <- klass_muni |>
    dplyr::arrange(.data$code, .data$validToInRequestedRange) |>
    dplyr::group_by(.data$code) |>
    dplyr::summarise(
      validFromInRequestedRange = min(.data$validFromInRequestedRange, na.rm = TRUE),
      validToInRequestedRange = max(.data$validToInRequestedRange, na.rm = TRUE),
      # Get the last valid name (last row after sorting by validToInRequestedRange)
      name = dplyr::last(.data$name),
      .groups = "drop"
    )

  return(klass_muni)
}
