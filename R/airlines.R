download_airlines <- function(data_dir, raw_dir, flights) {
  col_types <- cols(
    Code = col_character(),
    Description = col_character()
  )
  url <- paste0("http://www.transtats.bts.gov/Download_Lookup.asp?",
                "Lookup=L_UNIQUE_CARRIERS")
  raw <- read_csv(url, col_types = col_types)
  airlines <- raw %>%
    rename_(carrier = ~Code, name = ~Description) %>%
    semi_join(flights, by = "carrier") %>%
    arrange_(~carrier)
  save_csv(airlines, file.path(raw_dir, "airlines.csv"))
  save_rda(airlines, file = file.path(data_dir, "airlines.rda"),
           compress = "bzip2")
  airlines
}

