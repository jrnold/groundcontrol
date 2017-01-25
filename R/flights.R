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
    Year = col_integer(),
    Quarter = col_integer(),
    Month = col_integer(),
    DayofMonth = col_integer(),
    DayOfWeek = col_integer(),
    FlightDate = col_date(format = ""),
    UniqueCarrier = col_character(),
    AirlineID = col_integer(),
    Carrier = col_character(),
    TailNum = col_character(),
    FlightNum = col_integer(),
    OriginAirportID = col_integer(),
    OriginAirportSeqID = col_integer(),
    OriginCityMarketID = col_integer(),
    Origin = col_character(),
    OriginCityName = col_character(),
    OriginState = col_character(),
    OriginStateFips = col_character(),
    OriginStateName = col_character(),
    OriginWac = col_integer(),
    DestAirportID = col_integer(),
    DestAirportSeqID = col_integer(),
    DestCityMarketID = col_integer(),
    Dest = col_character(),
    DestCityName = col_character(),
    DestState = col_character(),
    DestStateFips = col_character(),
    DestStateName = col_character(),
    DestWac = col_integer(),
    CRSDepTime = col_integer(),
    DepTime = col_integer(),
    DepDelay = col_double(),
    DepDelayMinutes = col_double(),
    DepDel15 = col_double(),
    DepartureDelayGroups = col_integer(),
    DepTimeBlk = col_character(),
    TaxiOut = col_double(),
    WheelsOff = col_character(),
    WheelsOn = col_character(),
    TaxiIn = col_double(),
    CRSArrTime = col_integer(),
    ArrTime = col_double(),
    ArrDelay = col_double(),
    ArrDelayMinutes = col_double(),
    ArrDel15 = col_double(),
    ArrivalDelayGroups = col_integer(),
    ArrTimeBlk = col_character(),
    Cancelled = col_double(),
    CancellationCode = col_character(),
    Diverted = col_double(),
    CRSElapsedTime = col_double(),
    ActualElapsedTime = col_double(),
    AirTime = col_double(),
    Flights = col_double(),
    Distance = col_double(),
    DistanceGroup = col_integer(),
    CarrierDelay = col_double(),
    WeatherDelay = col_double(),
    NASDelay = col_double(),
    SecurityDelay = col_double(),
    LateAircraftDelay = col_double(),
    FirstDepTime = col_character(),
    TotalAddGTime = col_double(),
    LongestAddGTime = col_double(),
    DivAirportLandings = col_integer(),
    DivReachedDest = col_double(),
    DivActualElapsedTime = col_double(),
    DivArrDelay = col_double(),
    DivDistance = col_double(),
    Div1Airport = col_character(),
    Div1AirportID = col_integer(),
    Div1AirportSeqID = col_integer(),
    Div1WheelsOn = col_character(),
    Div1TotalGTime = col_double(),
    Div1LongestGTime = col_double(),
    Div1WheelsOff = col_character(),
    Div1TailNum = col_character(),
    Div2Airport = col_character(),
    Div2AirportID = col_integer(),
    Div2AirportSeqID = col_integer(),
    Div2WheelsOn = col_integer(),
    Div2TotalGTime = col_double(),
    Div2LongestGTime = col_double(),
    Div2WheelsOff = col_integer(),
    Div2TailNum = col_character(),
    Div3Airport = col_character(),
    Div3AirportID = col_character(),
    Div3AirportSeqID = col_character(),
    Div3WheelsOn = col_character(),
    Div3TotalGTime = col_character(),
    Div3LongestGTime = col_character(),
    Div3WheelsOff = col_character(),
    Div3TailNum = col_character(),
    Div4Airport = col_character(),
    Div4AirportID = col_character(),
    Div4AirportSeqID = col_character(),
    Div4WheelsOn = col_character(),
    Div4TotalGTime = col_character(),
    Div4LongestGTime = col_character(),
    Div4WheelsOff = col_character(),
    Div4TailNum = col_character(),
    Div5Airport = col_character(),
    Div5AirportID = col_character(),
    Div5AirportSeqID = col_character(),
    Div5WheelsOn = col_character(),
    Div5TotalGTime = col_character(),
    Div5LongestGTime = col_character(),
    Div5WheelsOff = col_character(),
    Div5TailNum = col_character(),
    X110 = col_character()
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
