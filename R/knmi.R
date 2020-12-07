#' @importFrom dplyr %>%

#' R wrapper for knmi daily data api
#' https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script
#'
#' @param start The start time
#' @param end The end time
#' @param vars The variables to query
#' @param stns The stations to query
#' @return A data.frame with reponse data
#' @export
knmi_get_data <- function(start, end, vars, stns) {
  r <- httr::POST(
    "http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi",
    body = list(
      start = start,
      end = end,
      vars = if(length(vars) > 1) vars %>% paste(collapse = ":") else vars,
      stns = if(length(stns) > 1) stns %>% paste(collapse = ":") else stns
    )
  )

  content <- httr::content(r, as = "raw")

  cols <- content %>%
    readr::read_lines() %>%
    purrr::keep(~stringr::str_detect(.x, "^# STN,YYYYMMDD")) %>%
    stringr::str_replace_all("[\\#\\s]+", "") %>%
    stringr::str_split(",") %>%
    dplyr::first()

  measurements <- content %>%
    readr::read_delim(
      ",",
      comment = "#",
      col_names = cols,
      col_types = readr::cols(YYYYMMDD = readr::col_date(format = "%Y%m%d"), .default = readr::col_integer()),
      trim_ws = TRUE
    )

  station_cols <- content %>%
    readr::read_lines() %>%
    purrr::keep(~stringr::str_detect(.x, "^# STN\\s+")) %>%
    stringr::str_replace_all("[#\\:]+", "") %>%
    stringr::str_trim() %>%
    stringr::str_split("\\s{2,}") %>%
    dplyr::first() %>%
    purrr::map(~(if(.x == "STN") .x else paste0("STN_", .x))) %>%
    as.character()

  stations <- content %>%
    readr::read_lines() %>%
    purrr::keep(~stringr::str_detect(.x, "^# \\d+\\:")) %>%
    stringr::str_replace_all("[#\\:]+", "") %>%
    stringr::str_replace_all("\\s{2,}", ",") %>%
    stringr::str_trim() %>%
    readr::read_delim(
      ",",
      col_names = station_cols,
      col_types = readr::cols(STN_NAME = readr::col_character(), STN = readr::col_integer(), .default = readr::col_number())
    )

  measurements %>%
    dplyr::left_join(stations)
}
