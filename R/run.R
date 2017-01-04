#' @importFrom stats time
#' @importFrom utils download.file
#' @importFrom utils unzip
#'
#' @import readr
#' @import dplyr
#' @importFrom devtools create
#' @importFrom devtools document
#' @importFrom devtools as.package
#' @importFrom devtools use_data_raw
#' @importFrom devtools use_build_ignore
#' @importFrom lubridate ymd
#' @importFrom lubridate mday
#' @importFrom lubridate month
#' @importFrom purrr map_chr
#' @importFrom purrr "%||%"
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom httr stop_for_status
#' @importFrom assertthat is.flag
#' @importFrom assertthat assert_that
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_c
NULL

airport_str <- function(airports, conjunction = "or") {
  if (length(airports) == 1) {
    airports
  } else {
    str_c(str_c(airports[-1], collapse = ", "),
          airports[length(airports)],
          sep = str_c(", ", conjunction))
  }
}

description_title <- function(airports, year, origin, dest) {
  str_c("Flights that ",
        str_c(if (origin) "Departed" else "",
                  if (dest) "Arrived at" else "",
                  sep = " or "),
        " ", airport_str(airports, "or"),
        " in ", year)
}

description_text <- function(airports, year, origin, dest) {
  str_c("Airline on-time data for all flights ",
        str_c(if (origin) "departing" else "",
              if (dest) "arriving" else "",
              sep = " or "),
        " ", airport_str(airports, "or"),
        " in ", year, ". ",
        "Also includes useful 'metadata' on airlines, airports, weather, and planes.",
        " This is a data package created by groundcontrol")
}

# copied from devtools:::is_dir
is_dir <- function (x) file.info(x)$isdir

# copied from devtools:::use_directory
use_directory <- function(path, ignore = FALSE, pkg = ".") {
  pkg <- as.package(pkg)
  pkg_path <- file.path(pkg$path, path)
  if (file.exists(pkg_path)) {
    if (!is_dir(pkg_path)) {
      stop("`", path, "` exists but is not a directory.",
           call. = FALSE)
    }
  }
  else {
    message("* Creating `", path, "`.")
    dir.create(pkg_path, showWarnings = FALSE, recursive = TRUE)
  }
  if (ignore) {
    message("* Adding `", path, "` to `.Rbuildignore`.")
    use_build_ignore(path, pkg = pkg)
  }
  invisible(TRUE)
}

#' From devtools:::render_template
#' https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r
render_template <- function(name, data = list()) {
  path <- system.file("templates", name, package = "devtools")
  template <- readLines(path)
  whisker::whisker.render(template, data)
}

#' @export
build_pkg <- function(path, airport_codes, year, origin = TRUE, dest = FALSE,
                      all_weather = FALSE, description = getOption("devtools.desc")) {
  description_ <- append(list(
    description = description_text(airport_codes, year, origin, dest),
    title = description_text(airport_codes, year, origin, dest),
    license = "CC0"
  ), description)
  create(path, description = description)
  pkg <- as.package(path)
  use_directory("data", ignore = FALSE, pkg = pkg)
  use_data_raw(path)
  download_flightdata(airport_codes, year,
                      data_dir = file.path(path, "data"),
                      raw_dir = file.path(path, "data-raw"),
                      origin = origin, dest = dest, all_weather = all_weather)
  use_directory("R", pkg = pkg)
  document(pkg)
}

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
  if (!dir.exists(data_dir)) dir.create(data_dir)
  if (!dir.exists(raw_dir)) dir.create(raw_dir)
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
