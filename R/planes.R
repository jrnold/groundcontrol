download_planes <- function(data_dir, raw_dir, flights) {
  dst_dir <- file.path(raw_dir, "planes")
  dir.create(dst_dir, showWarnings = FALSE, recursive = TRUE)
  # Update URL from
  # http://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download/
  # src <- "http://registry.faa.gov/database/AR062014.zip"
  src <- "http://registry.faa.gov/database/ReleasableAircraft.zip"
  if (!file.exists(file.path(dst_dir, "MASTER.txt"))) {
    tmp <- tempfile(fileext = ".zip")
    r <- GET(src, write_disk(tmp))
    stop_for_status(r)
    unzip(tmp, exdir = dst_dir, junkpaths = TRUE)
    message("Unzipped contents to ", dst_dir)
  }
  col_types = cols(
    .default = col_character(),
    `YEAR MFR` = col_integer(),
    `TYPE REGISTRANT` = col_integer(),
    `LAST ACTION DATE` = col_integer(),
    `CERT ISSUE DATE` = col_integer(),
    `TYPE AIRCRAFT` = col_integer(),
    `TYPE ENGINE` = col_integer(),
    `MODE S CODE` = col_integer(),
    `AIR WORTH DATE` = col_integer(),
    `EXPIRATION DATE` = col_integer()
  )
  master <- read_csv(file.path(dst_dir, "MASTER.txt"),
                     trim_ws = TRUE, col_types = col_types)
  names(master) <- str_replace_all(str_to_lower(names(master)), "[- ()]", ".")

  keep <- master %>%
    select_(nnum = ~ n.number,
            code = ~ mfr.mdl.code,
            year = ~ year.mfr)

  col_types <- cols(
    .default = col_character(),
    MFR = col_character(),
    MODEL = col_character(),
    `TYPE-ACFT` = col_integer(),
    `TYPE-ENG` = col_integer(),
    `AC-CAT` = col_integer(),
    `BUILD-CERT-IND` = col_integer(),
    `NO-ENG` = col_character(),
    `NO-SEATS` = col_character(),
    `AC-WEIGHT` = col_character(),
    SPEED = col_character(),
    X12 = col_character()
  )

  ref <- read_csv(file.path(dst_dir, "ACFTREF.txt"), trim_ws = TRUE,
                  col_types = col_types)
  names(ref)[1] <- "CODE"
  names(ref) <- str_replace_all(str_to_lower(names(ref)), "[- ()]", ".")

  ref <- ref %>%
    select_(~code,
            ~mfr,
            ~model,
            ~type.acft,
            ~type.eng,
            ~no.eng,
            ~no.seats,
            ~speed)

  # Combine together

  all <- keep %>%
    inner_join(ref, by = "code") %>%
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

  planes <-
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
    save_csv(planes, file.path(raw_dir, "planes.csv"))
    save_rda(planes, file = file.path(data_dir, "planes.rda"),
             compress = "bzip2")
}
