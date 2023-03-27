#' Retrieve municipality classifications on a yearly basis from SSB
#'
#' @param aar Integer vector with the years for which you want valid classifications.
#'
#' @return A data frame with municipality name, number and year of validity
#' @export
#'
#' @examples
#' kommunestruktur_2020 <- kmn_inndeling(aar = 2020)
#' head(kommunestruktur_2020)
#'
#' @importFrom rlang .data
#'
kmn_inndeling <- function(aar) {

  datoer <- paste0(aar, "-01-01")

  names(datoer) <- aar

  kmn_inndeling <-
    purrr::map_dfr(
    datoer,
    ~ klassR::GetKlass(131, date = .x),
    .id = "aar"
  ) |>
    dplyr::rename(
      "knavn" = .data$name,
      "knr" = .data$code
    ) |>
    dplyr::select(.data$knavn, .data$knr, .data$aar) |>
    dplyr::filter(.data$knr != "9999")

  return(kmn_inndeling)

}
