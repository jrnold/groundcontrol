#' @importFrom readr write_csv read_csv
#' @importFrom lubridate ymd mday month
#' @import dplyr
#' @importFrom purrr "%||%"
#' @importFrom stats time
#' @importFrom utils download.file unzip
#' @importFrom httr GET write_disk stop_for_status
#' @importFrom assertthat is.flag assert_that
NULL

cachedir <- function(...) {
  d <- file.path(tempdir(), ".groundcountrol", ...)
  if (!file.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
  d
}

cachefile <- function(...) {
  file.path(tempdir(), ".groundcontrol", ...)
}


#' @export
download_flightdata <- function(airport_codes, year,
                                 raw_dir = "./data",
                                 data_dir = "./data-raw",
                                 origin = TRUE,
                                 dest = FALSE,
                                 cache = raw_dir) {
  dir.create(raw_dir)
  dir.create(data_dir)

  airports <- download_airports(cache)
  write_csv(airports, file.path(raw_dir, "airports.csv"))
  save(airports, file = file.path(data_dir, "airports.rda"),
       compress = "bzip2")

  flights <- download_flights(year,
                              airport_codes,
                              cache = raw_dir,
                              is_origin = origin,
                              is_dest = dest)
  write_csv(flights, file.path(raw_dir, "flights.csv"))
  save(flights, file = file.path(data_dir, "flights.rda"),
       compress = "bzip2")

  weather <- download_weather(year, airport_codes,
                              cache = cache)
  write_csv(weather, file.path(raw_dir, "weather.csv"))
  save(weather, file = file.path(data_dir, "weather.rda"),
       compress = "bzip2")

  airlines <- download_airlines(flights)
  write_csv(airlines, file.path(raw_dir, "airlines.csv"))
  save(airlines, file = file.path(data_dir, "airlines.rda"),
       compress = "bzip2")

  planes <- download_planes(flights)
  write_csv(planes, file.path(raw_dir, "planes.csv"))
  save(planes, file = file.path(data_dir, "planes.rda"),
       compress = "bzip2")

}
