flight_url <- function(year, month) {
  base_url <- "http://tsdata.bts.gov/PREZIP/"
  sprintf(paste0(base_url, "On_Time_On_Time_Performance_%d_%d.zip"), year, month)
}

flights_download_month <- function(month, year, cache) {
  dst <- file.path(cache, paste0(year, "-", month, ".csv"))
  if (!file.exists(dst)) {
    url <- flight_url(year, month)

    temp <- tempfile(fileext = ".zip")
    download.file(url, temp)

    files <- unzip(temp, list = TRUE)
    # Only extract biggest file
    csv <- files$Name[order(files$Length, decreasing = TRUE)[1]]

    unzip(temp, exdir = cache, junkpaths = TRUE, files = csv)

    src <- file.path(cache, csv)

    file.rename(src, dst)
  }
}


flights_download_year <- function(year, cache) {
  months <- 1:12
  lapply(months, flights_download_month, year, cache)
}


get_flights_for_airports <- function(path, airports) {
  col_types <- cols(
    DepTime = col_integer(),
    ArrTime = col_integer(),
    CRSDepTime = col_integer(),
    CRSArrTime = col_integer(),
    Carrier = col_character(),
    UniqueCarrier = col_character()
  )
  read_csv(path, col_types = col_types) %>%
    select_(
      year = ~Year, month = ~Month, day = ~DayofMonth,
      dep_time = ~DepTime, sched_dep_time = ~CRSDepTime, dep_delay = ~DepDelay,
      arr_time = ~ArrTime, sched_arr_time = ~CRSArrTime, arr_delay = ~ArrDelay,
      carrier = ~Carrier, flight = ~FlightNum, tailnum = ~TailNum,
      origin = ~Origin, dest = ~Dest,
      air_time = ~AirTime, distance = ~Distance
    ) %>%
    filter_(~origin %in% airports) %>%
    mutate_(
      hour = ~ sched_dep_time %/% 100,
      minute = ~ sched_dep_time %% 100,
      time_hour = ~ lubridate::make_datetime(year, month, day, hour, 0, 0)
    ) %>%
    arrange_(~year, ~month, ~day, ~dep_time)
}

download_flights <- function(year, airports, cache = NULL) {
  if (is.null(cache)) {
    cache <- tempfile()
    dir.create(cache)
  }
  flights_download_year(year, cache)
  all <- lapply(dir(cache, full.names = TRUE),
                get_flights_for_airports, airports)
  flights <- bind_rows(all)
  flights$tailnum[flights$tailnum == ""] <- NA
  flights
}
