#' Filter Municipality-Year Combinations by Validity
#'
#' Removes municipality-year combinations that are not valid according to the official SSB municipality structure.
#' The function checks if the municipality is valid for the entire year of observation, i.e.,
#' the municipality's validity period must cover the full calendar year for a row to be retained.
#'
#' @param data A data frame with municipality observations per year. Must contain a column for municipality ID (Four numbers in character fromat) and the year of observervation (numeric or integer). The function assumes that the observation is recorded at the end of the calendar year.
#' @param municipality_structure Optional. A data frame with columns: `code`, `valid_from`, and `valid_to`. If not supplied, the function will call `get_municipality_structure()` to retrieve the structure for the period covering the years in `data`.
#' @param code_col Name of the column in `data` containing municipality IDs (character). Default: "code".
#' @param year_col Name of the column in `data` containing years (numeric/integer). Default: "year".
#'
#' @return A filtered data frame containing only valid municipality-year combinations.
#'
#' @details
#' This function checks each municipality code against the validity period defined in the `municipality_structure` data frame. For a municipality-year combination to be retained, the municipality must be valid for the entire year (from January 1st to December 31st of the given year). Also drops municipality codes that are not present in the `municipality_structure`.
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

filter_municipality_structure <- function(data, municipality_structure,
                                         code_col = "code", year_col = "year") {
  # Input validation
  if (!code_col %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data.", code_col))
  }
  if (!year_col %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data.", year_col))
  }

  # Check if municipality_structure is provided, if not, fetch it
  if (missing(municipality_structure)) {
    start_year <- min(data[[year_col]], na.rm = TRUE)
    end_year <- max(data[[year_col]], na.rm = TRUE)
    municipality_structure <- get_municipality_structure(start_year, end_year)
  }

  # Ensure the structure has the required columns
  required_cols <- c("code", "valid_from", "valid_to")
  if (!all(required_cols %in% colnames(municipality_structure))) {
    stop("municipality_structure must contain 'code', 'valid_from', and 'valid_to' columns.")
  }

  # Prepare data for merging
  data_for_merge <- data.frame(
    code = as.character(data[[code_col]]),
    year = as.integer(data[[year_col]]),
    stringsAsFactors = FALSE
  )

  # Merge data with municipality structure
  merged <- merge(data_for_merge, municipality_structure, by = "code")
  # Ensure valid_from and valid_to are in Date format
  merged$valid_from <- as.Date(merged$valid_from)
  merged$valid_to <- as.Date(merged$valid_to)

  # Calculate start and end date for each year observation
  year_start <- as.Date(paste0(merged$year, "-01-01"))
  year_end   <- as.Date(paste0(merged$year, "-12-31"))

  # Keep only rows where the municipality is valid for the entire year
  keep <- merged$valid_from <= year_start & merged$valid_to > year_end
  valid <- merged[keep, , drop = FALSE]

  # Remove unnecessary columns
  valid$valid_from <- NULL
  valid$valid_to <- NULL

  return(valid)
}
