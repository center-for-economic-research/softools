#' Hent data fra SSB
#'
#' Denne funksjonen laster ned data fra Statistisk sentralbyrå (SSB) via deres API.
#' Funksjonen er laget for å hente data for kommuner.
#' Dokumentasjon om API finnes her: https://www.ssb.no/api/api-mot-statistikkbanken--brukerveiledning
#'
#' @param ssb_tbl_id Character. ID for tabellen som skal hentes fra SSB.
#' @param aar Character. Ett eller flere årstall som data skal hentes for.
#' @param statistikkvariabel Character. Kode for statistikkvariabelen som skal hentes.
#' @param kommuner Character vector. En vektor med kommunenummer (fire siffer).
#' @param params List. Valgfrie parametere for API-kallet.
#'
#' @return En data.frame med data fra SSB.
#'
#' @details
#' Merk: Dersom argumentet `makeNAstatus = TRUE` benyttes i PxWebApiData::ApiData (som denne funksjonen gjør), forsøker funksjonen å legge til en kolonne `NAstatus` i datasettet. Dette er ikke garantert for alle tabeller: Om tabellen fra SSB ikke inneholder nødvendig metadata, vil kolonnen ikke bli lagt til. Funksjonen sjekker derfor alltid om `NAstatus` finnes før den brukes. Dersom kolonnen finnes, fjernes rader der `NAstatus == "."` (som vanligvis betyr at det ikke finnes observasjon for enheten). Hvis ikke, gis det en melding, og ingen rader fjernes basert på NAstatus.
#'
#' Dette betyr at tilstedeværelsen av `NAstatus`-kolonnen ikke er stabil, og brukere bør ikke forvente at den alltid er tilgjengelig i resultatet.
#'
#' @importFrom httr GET content
#' @importFrom PxWebApiData ApiData
#'
#' @examples
#' # Hent befolkningstall for kommuner i 2020 fra SSB tabell "07459"
#' # (forutsetter at PxWebApiData og httr er installert og lastet)
#' dta <- get_ssb_dta(
#'   ssb_tbl_id = "07459",
#'   aar = "2020",
#'   statistikkvariabel = "Personer",
#'   kommuner = c("0301", "1103", "5001")
#' )
#' head(dta)
#'
#' # Merk: Dette eksempelet forutsetter at tabell-id og variabelnavn er gyldige for valgt aar.
#'
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

  # Fjern variabel "Tid", inneholder samme info som "aar"
      ssb_tbl <-
        ssb_tbl[, !grepl("Tid", colnames(ssb_tbl))]
    } else {
      # Remove vars that start with a capitalized letter
      # These are metadata vars in tables not from KOSTRA
      # TODO: Undersok om jeg bor vaere mer spesifikk
      # Kun generelt dersom det er slik at metadata navn varierer etter tabell
      ssb_tbl <-
        ssb_tbl[, !grepl("^[A-Z]", colnames(ssb_tbl))
                # Don't remove Region, angir kommunenummer
                | grepl("Region", colnames(ssb_tbl))
                # Don't remove "NAstatus", angir status for manglende data
                | grepl("NAstatus", colnames(ssb_tbl))
                ]
    }

    return(ssb_tbl)
  }
