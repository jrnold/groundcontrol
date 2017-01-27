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
  col_names <- c(
      "N-NUMBER",        "SERIAL NUMBER",   "MFR MDL CODE",
      "ENG MFR MDL",     "YEAR MFR",        "TYPE REGISTRANT",
      "NAME",            "STREET",          "STREET2",
      "CITY",            "STATE",           "ZIP CODE",
      "REGION",          "COUNTY",          "COUNTRY",
      "LAST ACTION DATE","CERT ISSUE DATE", "CERTIFICATION",
      "TYPE AIRCRAFT",   "TYPE ENGINE",     "STATUS CODE",
      "MODE S CODE",     "FRACT OWNER",     "AIR WORTH DATE",
      "OTHER NAMES(1)",  "OTHER NAMES(2)",  "OTHER NAMES(3)",
      "OTHER NAMES(4)",  "OTHER NAMES(5)",  "EXPIRATION DATE",
      "UNIQUE ID",       "KIT MFR",         "KIT MODEL",
      "MODE S CODE HEX", "X35")
  col_types <- cols(
    .default = col_character(),
    `N-NUMBER` = col_character(),
    `SERIAL NUMBER` = col_character(),
    `MFR MDL CODE` = col_character(),
    `ENG MFR MDL` = col_character(),
    `YEAR MFR` = col_integer(),
    `TYPE REGISTRANT` = col_integer(),
    NAME = col_character(),
    STREET = col_character(),
    STREET2 = col_character(),
    CITY = col_character(),
    STATE = col_character(),
    `ZIP CODE` = col_character(),
    REGION = col_character(),
    COUNTY = col_character(),
    COUNTRY = col_character(),
    `LAST ACTION DATE` = col_integer(),
    `CERT ISSUE DATE` = col_integer(),
    CERTIFICATION = col_character(),
    `TYPE AIRCRAFT` = col_integer(),
    `TYPE ENGINE` = col_integer(),
    `STATUS CODE` = col_character(),
    `MODE S CODE` = col_integer(),
    `FRACT OWNER` = col_character(),
    `AIR WORTH DATE` = col_integer(),
    `OTHER NAMES(1)` = col_character(),
    `OTHER NAMES(2)` = col_character(),
    `OTHER NAMES(3)` = col_character(),
    `OTHER NAMES(4)` = col_character(),
    `OTHER NAMES(5)` = col_character(),
    `EXPIRATION DATE` = col_integer(),
    `UNIQUE ID` = col_character(),
    `KIT MFR` = col_character(),
    `KIT MODEL` = col_character(),
    `MODE S CODE HEX` = col_character(),
    X35 = col_character()
  )
  master <- read_csv(file.path(dst_dir, "MASTER.txt"),
                     trim_ws = TRUE,
                     skip = 1,
                     col_names = col_names,
                     col_types = col_types)
  names(master) <- str_replace_all(str_to_lower(names(master)), "[- ()]", ".")

  keep <- master %>%
    select_(nnum = ~ n.number,
            code = ~ mfr.mdl.code,
            year = ~ year.mfr)
  col_names <-
    c("CODE", "MFR", "MODEL", "TYPE-ACFT",
      "TYPE-ENG", "AC-CAT", "BUILD-CERT-IND", "NO-ENG",
      "NO-SEATS", "AC-WEIGHT", "SPEED", "X12")
  col_types <- cols(
    .default = col_character(),
    `CODE` = col_character(),
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
  ref <- read_delim(file.path(dst_dir, "ACFTREF.txt"), ",",
                    quote = "",
                    skip = 1,
                    trim_ws = TRUE,
                    col_names = col_names,
                    col_types = col_types)
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

  engine <- c("None", "Reciprocating", "Turbo-prop", "Turbo-shaft",
              "Turbo-jet", "Turbo-fan", "Ramjet", "2 Cycle", "4 Cycle",
              "Unknown", "Electric", "Rotary")
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
