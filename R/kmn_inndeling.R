#' Get municipality classifications from SSB
#'
#' @param datoer
#' String for the required date of the classification. Format must be "yyyy-mm-dd". For an inverval, provide two dates as a vector. If blank, will default to today's date
#'
#'
#' @return A data frame with municipality name, number and year of validity
#' @export
#'
#' @examples
#' kommunestruktur_2020 <- kmn_inndeling(datoer = "2020-01-01")
#' head(kommunestruktur_2020)
#'
#' @importFrom rlang .data
#'
kmn_inndeling <- function(datoer) {

  names(datoer) <- datoer
  kmn_inndeling <-
    purrr::map_dfr(
    datoer,
    ~ klassR::GetKlass(131, date = .x),
    .id = "dato"
  ) |>
    dplyr::rename(
      "knavn" = .data$name,
      "knr" = .data$code
    ) |>
    dplyr::select(.data$knavn, .data$knr, .data$dato)

  return(kmn_inndeling)

}
