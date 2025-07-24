#' Hent data fra SSB
#'
#' Denne funksjonen laster ned data fra Statistisk sentralbyrå (SSB) via deres API.
#' Funksjonen er laget for å hente data for kommuner.
#'
#' @param ssb_tbl_id Character. ID for tabellen som skal hentes fra SSB.
#' @param aar Character. Ett eller flere årstall som data skal hentes for.
#' @param statistikkvariabel Character. Kode for statistikkvariabelen som skal hentes.
#' @param kommuner Character vector. En vektor med kommunenummer (fire siffer).
#' @param params List. Valgfrie parametere for API-kallet.
#'
#' @return En data.frame med data fra SSB.
#'
#' @importFrom httr GET content
#' @importFrom PxWebApiData ApiData
#' @export
get_ssb_dta <-
  function(ssb_tbl_id,
           aar, # Enkelt år eller vektor med alle ønskede år, må være character
           statistikkvariabel,
           kommuner, # Kommunenummer, fire siffer, vektor
           # Brukes dersom det er tabellspesifikke parametre
           params = NULL) {

    # Get table title
    url_tbl <- paste0("https://data.ssb.no/api/v0/no/table/", ssb_tbl_id)
    resp <- httr::GET(url_tbl)
    title <- httr::content(resp)$title

    # Print years and table id
    message(paste(
      "Henter data for",
      paste(aar, collapse = ", "),
      "fra SSB tabell",
      title
    ))

    # Construct API call
    # TODO: legg inn sjekk for at API call er gyldig. Tror dette kan
    # Gjøres på flere måter, men en start er å sjekke om tabell finnes,
    # Sjekke om årene finnes i tabell etc...
    api_call <-
      list(
        ssb_tbl_id,
        ContentsCode = statistikkvariabel,
        Tid = aar,
        # Hvilke regioner vi skal hente data for
        # bruker unnamed argument for å unngå å sende med navn på regionvariabel
        # da denne er forskjellig fra KOSTRA tabeller og andre tabeller
        kommuner,
        # Returner både data og metadata, fordi regionkode = kommunenummer
        returnDataSet = 12
      )

    # Include params if provided
    if (!is.null(params)) {
      api_call <- c(api_call, params)
    }

    # Send API call og hent data
    ssb_tbl <-
      do.call(PxWebApiData::ApiData, api_call)

    # Finn kolonne med kommunenummer i ssb-tabell
    # Denne kan endre seg fra tabell til tabell
    kommunenummer_kolonne <-
      # De fleste tabeller har "region" som kolonnenavn for regionnummer
      grep("region", colnames(ssb_tbl))

    # Velg førse kolonne som inneholder "region"
    kommunenummer_kolonne <-
      kommunenummer_kolonne[1]

    # Antall unike kommunenummer i SSB tabell
    length_kommuner_ssb <-
      length(unique(ssb_tbl[, kommunenummer_kolonne]))

    # Antall unike kommunenummer i gjeldende kommunestruktur
    #length_kommuner_klass <- length(kommuner_gjeldende$code)

    # Sjekk at alle kommuner er med
    # if (length_kommuner_ssb != length_kommuner_klass) {
    #   stop(paste(
    #     "Ikke alle kommuner er med i tallene for",
    #     aar,
    #     "Fikk:",
    #     length_kommuner_ssb,
    #     ": forventet",
    #     length_kommuner_klass
    #   ))
    # }
    #
    # Fjern variabelen "ContentsCode"
    # metadata for statistikkvariabel
    ssb_tbl <-
      ssb_tbl[, !grepl("ContentsCode", colnames(ssb_tbl))]

    # Fjern vars for KOSTRA tabell
    # Sjekk først om det er vars med "KOK"
    if (any(grepl("KOK", colnames(ssb_tbl)))) {
      # Fjern vars
      ssb_tbl <-
        # vars med "KOK",
        # metadata, gir kodeverdi av variabler
        ssb_tbl[, !grepl("KOK", colnames(ssb_tbl)) |
                  # ikke fjern "KOKkommuneregion0000", angir kommunenummer
                  grepl("KOKkommuneregion0000", colnames(ssb_tbl))]

      # Fjern variabel "Tid", inneholder samme info som "år"
      ssb_tbl <-
        ssb_tbl[, !grepl("Tid", colnames(ssb_tbl))]
    } else {
      # Remove vars that start with a capitalized letter
      # These are metadata vars in tables not from KOSTRA
      ssb_tbl <-
        ssb_tbl[, !grepl("^[A-Z]", colnames(ssb_tbl))
                # Don't remove Region, angir kommunenummer
                | grepl("Region", colnames(ssb_tbl))]
    }

    return(ssb_tbl)
  }
