#' Filter Municipality-Year Combinations by Validity
#'
#' Removes municipality-year combinations that are not valid according to the official SSB municipality structure.
#'
#' @param data A data frame with two columns: `code` (municipality ID as character) and `year` (numeric or integer).
#' @param municipality_structure Optional. A data frame with columns: `code`, `valid_from`, and `valid_to`. If not supplied, the function will call `get_municipality_structure()` to retrieve the structure for the period covering the years in `data`.
#'
#' @return A filtered data frame containing only valid municipality-year combinations.
#'
#' @details
#' This function checks each municipality code against the validity period defined in the `municipality_structure` data frame. If a municipality code is not valid for the specified year, it will be removed from the output.
#' Also drops municipality codes that are not present in the `municipality_structure`.
#'
#' @examples
#' \dontrun{
#' # Example data frame with municipality codes and years
#' # Trondheim (1601) and Moss (3002) were not valid in 2018 and 2022, respectively.
#' # Oslo (0301) and Halden (3001) are valid in 2020 and 2021.
#' data <- data.frame(
#'   code = c("0301", "1601", "3001", "3002"),
#'   year = c(2020, 2018, 2021, 2022)
#' )
#' # Filter out invalid municipality codes based on structure
#' filtered_data <- filter_municipality_structure(data)
#' # Only the valid municipality-year combinations will remain:
#' #   code year
#' # 1 0301 2020
#' # 2 3001 2021
#'
#' # Using a custom municipality structure
#' # municipality_structure <- get_municipality_structure(2018, 2022)
#' # filtered_data <- filter_municipality_structure(data, municipality_structure)
#' }
#'
#' @export
filter_municipality_structure <- function(data, municipality_structure) {
  # Check if municipality_structure is provided, if not, fetch it
  if (missing(municipality_structure)) {
    start_year <- min(data$year)
    end_year <- max(data$year)
    municipality_structure <- get_municipality_structure(start_year, end_year)
  }

  # Ensure the structure has the required columns
  required_cols <- c("code", "valid_from", "valid_to")
  if (!all(required_cols %in% colnames(municipality_structure))) {
    stop("municipality_structure must contain 'code', 'valid_from', and 'valid_to' columns.")
  }

  # Merge data with municipality structure
  merged <- merge(data, municipality_structure, by = "code")
  # Ensure valid_from and valid_to are in Date format
  merged$valid_from <- as.Date(merged$valid_from)
  merged$valid_to <- as.Date(merged$valid_to)

  # Filter rows where year is within the valid range using standard subsetting
  keep <- merged$year >= as.numeric(format(merged$valid_from, "%Y")) &
          merged$year < as.numeric(format(merged$valid_to, "%Y"))
  valid <- merged[keep, , drop = FALSE]

  # Remove unnecessary columns
  valid$valid_from <- NULL
  valid$valid_to <- NULL

  return(valid)
}
