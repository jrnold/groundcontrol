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

.PACKAGE <- "groundcontrol"

airport_str <- function(airports, conjunction = "or") {
  if (length(airports) == 1) {
    airports
  } else {
    str_c(str_c(airports[-1], collapse = ", "),
          airports[length(airports)],
          sep = str_c(", ", conjunction))
  }
}

title_text <- function(airports, year, origin, dest) {
  str_c("Flights that ",
        str_c(if (origin) "Departed from" else "",
                  if (dest) "Arrived at" else "",
                  sep = " or "),
        " ", airport_str(airports, "or"),
        " in ", year)
}

description_text <- function(airports, year, origin, dest) {
  str_c("Airline on-time data for all flights ",
        str_c(if (origin) "departing from" else "",
              if (dest) "arriving at" else "",
              sep = " or "),
        " ", airport_str(airports, "or"),
        " in ", year, ". ",
        "Also includes useful 'metadata' on airlines, airports, weather, and planes.",
        " This is a data package created by groundcontrol.")
}

# copied from devtools:::is_dir
is_dir <- function(x) file.info(x)$isdir

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

template_path <- function(name) {
  system.file("templates", name, package = .PACKAGE)
}

# From devtools:::render_template
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r
render_template <- function(name, file = NULL, data = list()) {
  template <- readLines(template_path(name))
  rendered <- whisker::whisker.render(template, data)
  if (!is.null(file)) {
    cat(rendered, file = file)
  }
  rendered
}

str_comma <- function(x, last = ", and ", sep = ", ") {
  n <- length(x)
  if (n < 2) {
    x
  } else {
    str_c(str_c(x[-1L], collapse = sep), sep = last)
  }
}


render_weather <- function(pkg, airports, year) {
  render_template("weather.R", file = file.path(pkg, "R", "weather.R"),
                  data = list(airports = str_comma(airports), year = year))
}

render_planes <- function(pkg) {
  file.copy(template_path("planes.R"), file.path(pkg, "R", "planes.R"))
}

render_airport <- function(pkg) {
  render_template("airport.R", file = file.path(pkg, "R", "airport.R"),
                  data = list(date = Sys.Date()))
}

render_airlines <- function(pkg) {
  render_template("airlines.R", file = file.path(pkg, "R", "airlines.R"),
                  data = list(date = Sys.Date()))
}

render_flights <- function(pkg, airports, year, origin, dest) {
  render_template("flights.R", file = file.path(pkg, "R", "flights.R"),
                  data = list(
                    airports = str_comma(airports, last = ", or "),
                    year = year,
                    depart = str_c(if (origin) "departing" else "",
                          if (dest) "arriving" else "",
                          sep = " or ")
                  )
  )
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

#' @param raw_dir Directory to save \code{.csv} and intermediate files.
#' @param data_dir Directory to save \code{.rda} file.
#' @rdname create_flights
#' @export
download_flightdata <- function(airport_codes,
                                 year,
                                 data_dir = "./data",
                                 raw_dir = "./data-raw",
                                 origin = TRUE,
                                 dest = FALSE,
                                 all_weather = FALSE) {
  flights <- NULL
  assert_that(is.flag(origin))
  assert_that(is.flag(dest))
  assert_that(origin || dest)
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

#' Create a data-only flights package
#'
#' Create a data-only R pacakge of flights data, like \pkg{nycflights13},
#' but with user-specified airports, years, and direction (departing, arriving).
#'
#' The function \code{create_flights} creates an R package with the flight data.
#' The function \code{download_flightdata} downloads the data and saves \code{.csv} and \code{.rda} files to specified directories, but does not create an R package.
#'
#' @param path Location to create new package. The last component of the path will be used as the package name.
#' @param author A \code{\link[utils]{person}} object to use as the author of the new package.
#' @param airport_codes A character vector of airport codes
#' @param year Year of flights to download.
#' @param origin,dest If \code{origin} (\code{dest}) is \code{TRUE}, keep all flights originating from (arriving at) \code{airports}. At least one of \code{origin} or \code{dest} must be \code{TRUE}.
#' @param all_weather If \code{TRUE}, get weather for not just the chosen airports, but also all (domestic) airports appearing in the flight data.
#' @return A \code{"package"} object for the created package.
#'  This function is called for
#'  the side-effect of downloading the data and creating a package.
#' @export
#' @examples
#' \dontrun{
#'   # Package with all flights departing from or arriving
#'   # at Seattle (SEA) in 2015.
#'   create_flights("seaflights15", "SEA", 2015, origin = TRUE, dest = TRUE)
#' }
create_flights <- function(path, airport_codes, year,
                           origin = TRUE, dest = FALSE,
                           all_weather = FALSE,
                           author = getOption("devtools.desc.author")) {
  assert_that(inherits(author, "person"))
  assert_that(is.flag(force))
  description <- list(
    Description = description_text(airport_codes, year, origin, dest),
    Title = title_text(airport_codes, year, origin, dest),
    License = "CC0",
    "Authors@R" = author
  )
  create(path, description = description)
  pkg <- as.package(path)
  use_directory("data", ignore = FALSE, pkg = pkg)
  use_data_raw(path)
  download_flightdata(airport_codes, year,
                      data_dir = file.path(path, "data"),
                      raw_dir = file.path(path, "data-raw"),
                      origin = origin, dest = dest, all_weather = all_weather)
  use_directory("R", pkg = pkg)
  render_flights(path, airport_codes, year, origin, dest)
  render_weather(path, airport_codes, year)
  render_planes(path)
  render_airport(path)
  render_airlines(path)
  document(pkg)
  pkg
}
