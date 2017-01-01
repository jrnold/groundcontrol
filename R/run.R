#' @importFrom readr write_csv read_csv
#' @importFrom lubridate ymd mday month
#' @import dplyr
#' @importFrom stats time
#' @importFrom utils download.file unzip
#' @importFrom httr GET write_disk stop_for_status
NULL

#' Create a flights data-only R package
#'
#' @param path Path to the location of the new package
#' @param year Year for flights
#' @param airports Airports to use for flights
#' @param description Options to add to the DESCRIPTION
#' @export
create <- function(path, year, airports,
                   description = getOption("devtools.desc")) {

  devtools::create(path, description = description)
  devtools::use_data_raw(path)
  download_all(path, airports, year)
}

download_all <- function(pkg, airports, year) {
  data_raw_dir <- file.path(pkg, "data-raw")
  csv_dir <- file.path(pkg, "data")
  airports <- download_airports()


  write_csv(airports, file.path(data_raw_dir, "airports.csv"))
  weather <- download_weather(year, airports)
  write_csv(weather, file.path(data_raw_dir, "weather.csv"))
  flights <- download_flights(year, airports)
  write_csv(weather, file.path(data_raw_dir, "flights.csv"))
  airlines <- download_airlines(flights)
  write_csv(airlines, file.path(data_raw_dir, "airlines.csv"))
  planes <- download_planes(flights)
  write_csv(planes, file.path(data_raw_dir, "planes.csv"))
  devtools::use_data(airports, flights, weather, airlines, planes,
                     pkg = pkg, overwrite = TRUE)
}
