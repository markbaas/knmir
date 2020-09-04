#' R wrapper for knmi daily data api
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
      vars = vars,
      stns = stns
    )
  )

  measurements <- httr::content(r, as = "raw") %>%
    readr::read_delim(
      ",",
      comment = "#",
      col_names = c("Station", "Date", "Temperature"),
      col_types = "ici",
      trim_ws = TRUE
    )

  stations <- httr::content(r, as = "raw") %>%
    readr::read_lines() %>%
    data.frame(X1 = .) %>%
    dplyr::filter(stringr::str_detect(X1, "# \\d+:")) %>%
    tidyr::separate(X1, into = c("Station", "Lat", "Lon", "Alt", "Name"), sep = "\\s{2,}") %>%
    dplyr::mutate(
      Station = Station %>% stringr::str_match("# (\\d+)\\:") %>% .[,2] %>% as.numeric(),
      Lon = as.numeric(Lon),
      Lat = as.numeric(Lat)
    )

  temp <- measurements %>%
    dplyr::left_join(stations)

  return(temp)
}
