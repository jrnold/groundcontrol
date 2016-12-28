#' @export
download_airports <- function(cache = tempfile()) {
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  dst <- file.path(cache, "airports.dat")
  if (!file.exists(dst)) {
    download.file(
      "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
      dst
    )
  }

  col_types <-
    cols(
      id = col_integer(),
      name = col_character(),
      city = col_character(),
      country = col_character(),
      faa = col_character(),
      icao = col_character(),
      lat = col_double(),
      lon = col_double(),
      alt = col_integer(),
      tz = col_double(),
      dst = col_character(),
      tzone = col_character()
    )
  raw <- read_csv(dst,
                  col_names = c("id", "name", "city", "country", "faa", "icao",
                                "lat", "lon", "alt", "tz", "dst", "tzone"),
                  col_types = col_types)

  raw %>%
    filter_(~country == "United States", ~faa != "") %>%
    select_(~faa, ~name, ~lat, ~lon, ~alt, ~tz, ~dst, ~tzone) %>%
    group_by_(~faa) %>% slice(1) %>% ungroup() # take first if duplicated
}
