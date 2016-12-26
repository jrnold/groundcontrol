library(dplyr)
library(readr)

download_planes <- function(flights, cache = NULL) {
  if (is.null(cache)) {
    cache <- tempfile()
    dir.create()
  }
  # Update URL from
  # http://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download/
  src <- "http://registry.faa.gov/database/AR062014.zip"

  if (!file.exists(lcl)) {
    tmp <- tempfile(fileext = ".zip")
    download.file(src, tmp)
    unzip(tmp, exdir = cache, junkpaths = TRUE)
  }

  master <- read_csv(file.path(cache, "MASTER.txt"), trim_ws = TRUE)
  names(master) <- tolower(names(master))

  keep <- master %>%
    tbl_df() %>%
    select(nnum = ~n.number, code = ~mfr.mdl.code, year = ~year.mfr)

  ref <- read_csv(file.path(cache, "ACFTREF.txt"), trim_ws = TRUE)
  names(ref) <- tolower(names(ref))

  ref <- ref %>%
    tbl_df() %>%
    select_(~code, ~mfr, ~model, ~type.acft, ~type.eng,
            ~no.eng, ~no.seats, ~speed)

  # Combine together

  all <- keep %>%
    inner_join(ref) %>%
    select_(~-code)
  all$speed[all$speed == 0] <- NA
  all$no.eng[all$no.eng == 0] <- NA
  all$no.seats[all$no.seats == 0] <- NA

  engine <- c("None", "Reciprocating", "Turbo-prop", "Turbo-shaft", "Turbo-jet",
              "Turbo-fan", "Ramjet", "2 Cycle", "4 Cycle", "Unknown", "Electric", "Rotary")
  all$engine <- engine[all$type.eng + 1]
  all$type.eng <- NULL

  acft <- c("Glider", "Balloon", "Blimp/Dirigible", "Fixed wing single engine",
            "Fixed wing multi engine", "Rotorcraft", "Weight-shift-control",
            "Powered Parachute", "Gyroplane")
  all$type <- acft[all$type.acft]
  all$type.acft <- NULL

  all$tailnum <- paste0("N", all$nnum)

  all %>%
    select_(
      ~ tailnum,
      ~ year,
      ~ type,
      manufacturer = ~ mfr,
      model = ~ model,
      engines = ~ no.eng,
      seats = ~ no.seats,
      ~ speed,
      ~ engine
    ) %>%
    semi_join(flights, "tailnum") %>%
    arrange_(~ tailnum)
}
