% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.R
\name{download_flightdata}
\alias{create_flights}
\alias{download_flightdata}
\title{Create a data-only flights package}
\usage{
download_flightdata(airport_codes, year, data_dir = "./data",
  raw_dir = "./data-raw", origin = TRUE, dest = FALSE,
  all_weather = FALSE)

create_flights(path, airport_codes, year, origin = TRUE, dest = FALSE,
  all_weather = FALSE, description = getOption("devtools.desc"))
}
\arguments{
\item{airport_codes}{A character vector of airport codes}

\item{year}{Year of flights to download.}

\item{data_dir}{Directory to save \code{.rda} file.}

\item{raw_dir}{Directory to save \code{.csv} and intermediate files.}

\item{origin, dest}{If \code{origin} (\code{dest}) is \code{TRUE}, keep all flights originating from (arriving at) \code{airports}. At least one of \code{origin} or \code{dest} must be \code{TRUE}.}

\item{all_weather}{If \code{TRUE}, get weather for not just the chosen airports, but also all (domestic) airports appearing in the flight data.}

\item{path}{Location to create new package. The last component of the path will be used as the package name.}

\item{description}{A \code{list} of description values to override default values or add additional values.}
}
\value{
A \code{"package"} object for the created package.
 This function is called for
 the side-effect of downloading the data and creating a package.
}
\description{
Create a data-only R pacakge of flights data, like \pkg{nycflights13},
but with user-specified airports, years, and direction (departing, arriving).
}
\details{
The function \code{create_flights} creates an R package with the flight data.
The function \code{download_flightdata} downloads the data and saves \code{.csv} and \code{.rda} files to specified directories, but does not create an R package.
}
\examples{
\dontrun{
  # Package with all flights departing from or arriving
  # at Seattle (SEA) in 2015.
  create_flights("seaflights15", "SEA", 2015, origin = TRUE, dest = TRUE)
}
}

