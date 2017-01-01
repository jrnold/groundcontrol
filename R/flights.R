flight_url <- function(year, month) {
  base_url <- "http://tsdata.bts.gov/PREZIP/"
  sprintf(paste0(base_url, "On_Time_On_Time_Performance_%d_%d.zip"),
          year, month)
}

flights_download_month <- function(month, year, cache_dir) {
  dst <- file.path(cache_dir, paste0(year, "-", month, ".csv"))
  if (!file.exists(dst)) {
    url <- flight_url(year, month)
    temp <- tempfile(fileext = ".zip")
    download.file(url, temp)
    files <- unzip(temp, list = TRUE)
    # Only extract biggest file
    csv <- files$Name[order(files$Length, decreasing = TRUE)[1]]
    unzip(temp, exdir = cache_dir, junkpaths = TRUE, files = csv)
    src <- file.path(cache_dir, csv)
    file.rename(src, dst)
    message("Saved file ", dst)
  }
  dst
}


flights_download_year <- function(year, cache_dir) {
  map_chr(1:12, function(month, year, cache_dir) {
    flights_download_month(month, year, cache_dir)
  }, year = year, cache_dir = cache_dir)
}


get_flights_for_airports <- function(path, airports,
                                     is_origin = TRUE,
                                     is_dest = FALSE) {
  assert_that(is.flag(is_origin))
  assert_that(is.flag(is_dest))
  assert_that(is_origin || is_dest)
  col_types <- cols(
    DepTime = col_integer(),
    ArrTime = col_integer(),
    CRSDepTime = col_integer(),
    CRSArrTime = col_integer(),
    Carrier = col_character(),
    UniqueCarrier = col_character()
  )
  flights <-
    read_csv(path, col_types = col_types) %>%
    select_(
      year = ~Year, month = ~Month, day = ~DayofMonth,
      dep_time = ~DepTime, sched_dep_time = ~CRSDepTime, dep_delay = ~DepDelay,
      arr_time = ~ArrTime, sched_arr_time = ~CRSArrTime, arr_delay = ~ArrDelay,
      carrier = ~Carrier, flight = ~FlightNum, tailnum = ~TailNum,
      origin = ~Origin, dest = ~Dest,
      air_time = ~AirTime, distance = ~Distance
    )

  if (is_origin & is_dest) {
    flights <- filter_(flights, ~ (origin %in% airports) | (dest %in% airports))
  } else if (is_origin) {
    flights <- filter_(flights, ~ (origin %in% airports))
  } else if (is_dest) {
    flights <- filter_(flights, ~ (dest %in% airports))
  }
  flights %>%
    mutate_(
      hour = ~ sched_dep_time %/% 100,
      minute = ~ sched_dep_time %% 100,
      time_hour = ~ lubridate::make_datetime(year, month, day, hour, 0, 0)
    ) %>%
    arrange_(~year, ~month, ~day, ~dep_time)
}

download_flights <- function(data_dir,
                             raw_dir,
                             year,
                             airports,
                             is_origin = TRUE,
                             is_dest = FALSE) {

  cache_dir <- file.path(raw_dir, "flights")
  dir.create(cache_dir, showWarnings = FALSE, recursive = FALSE)
  monthly_files <- flights_download_year(year, cache_dir)
  all <- lapply(monthly_files,
                get_flights_for_airports,
                airports,
                is_origin = is_origin,
                is_dest = is_dest)
  flights <- bind_rows(all)
  flights$tailnum[flights$tailnum == ""] <- NA
  csv_filename <- file.path(raw_dir, "flights.csv")
  save_csv(flights, csv_filename)
  dta_filename <- file.path(data_dir, "flights.rda")
  save_rda(flights, file = dta_filename, compress = "bzip2")
  flights
}
