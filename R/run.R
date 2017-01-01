#' @import readr
#' @import dplyr
#' @importFrom lubridate ymd mday month
#' @importFrom purrr "%||%" map_chr
#' @importFrom stats time
#' @importFrom utils download.file unzip
#' @importFrom httr GET write_disk stop_for_status
#' @importFrom assertthat is.flag assert_that
#' @importFrom stringr str_replace_all str_to_lower
NULL

save_csv <- function(x, path, ...) {
  write_csv(x, path, ...)
  message("Saved file ", path)
}

save_rda <- function(..., file = stop("File must be specified"),
                     envir = parent.frame()) {
  save(..., file = file, envir = envir)
  message("Saved file ", file)
}

#' @export
download_flightdata <- function(airport_codes,
                                 year,
                                 data_dir = "./data",
                                 raw_dir = "./data-raw",
                                 origin = TRUE,
                                 dest = FALSE,
                                 all_weather = FALSE) {
  flights <- NULL
  dir.create(raw_dir)
  dir.create(data_dir)
  download_flights(data_dir,
                    raw_dir,
                    year,
                    airport_codes,
                    is_origin = origin,
                    is_dest = dest)
  load(file.path(data_dir, "flights.rda"))
  download_airports(data_dir, raw_dir)
  if (all_weather) {
    all_airports <- unique(c(unique(flights[["origin"]]),
                             unique(flights[["dest"]])))
  } else {
    all_airports <- airport_codes
  }
  download_weather(data_dir, raw_dir, year, all_airports)
  download_airlines(data_dir, raw_dir, flights)
  download_planes(data_dir, raw_dir, flights)
  NULL
}
