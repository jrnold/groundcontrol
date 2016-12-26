#' @import dplyr
#' @import readr
NULL

download_airlines <- function(flights) {
  raw <- read_csv("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS")

  airlines <- raw %>%
    rename_(carrier = ~Code, name = ~Description) %>%
    semi_join(flights, by = "carrier") %>%
    arrange_(~carrier)
}

