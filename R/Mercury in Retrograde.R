# =========================================
# Mercury Retrograde Generator (2000–2100)
# Timezone-safe, interval-correct version
# =========================================

# install.packages("swephR")  # if needed
library(swephR)

# ----------------------------
# CONFIG
# ----------------------------

compute_tz <- "UTC"
display_tz <- "America/Los_Angeles"

start_date <- as.POSIXct("2000-01-01 00:00:00", tz = compute_tz)
end_date <- as.POSIXct("2100-12-31 23:00:00", tz = compute_tz)

# hourly resolution (important for boundary accuracy)
step <- "hour"

dates <- seq(start_date, end_date, by = step)

# ----------------------------
# MERCURY SPEED FUNCTION
# ----------------------------

get_mercury_speed <- function(dt) {
  year <- as.numeric(format(dt, "%Y"))
  month <- as.numeric(format(dt, "%m"))
  day <- as.numeric(format(dt, "%d"))

  hour_decimal <-
    as.numeric(format(dt, "%H")) +
    as.numeric(format(dt, "%M")) / 60

  jd <- swephR::swe_julday(
    year,
    month,
    day,
    hour_decimal,
    1 # Gregorian calendar
  )

  res <- swephR::swe_calc_ut(
    jd,
    2, # Mercury
    2 + 256 # Swiss Ephemeris + speed
  )

  # xx[4] = longitudinal speed (deg/day)
  res$xx[4]
}

# ----------------------------
# COMPUTE RETROGRADE FLAG
# ----------------------------

pb <- txtProgressBar(min = 1, max = length(dates), style = 3)

speed <- numeric(length(dates))

for (i in seq_along(dates)) {
  speed[i] <- get_mercury_speed(dates[i])

  if (i %% 1000 == 0) {
    setTxtProgressBar(pb, i)
  }
}

close(pb)
retrograde <- speed < 0

# ----------------------------
# GROUP INTO INTERVALS
# ----------------------------

r <- rle(retrograde)

ends <- cumsum(r$lengths)
starts <- c(1, head(ends, -1) + 1)

intervals <- data.frame(
  start = as.POSIXct(character()),
  end = as.POSIXct(character())
)

for (i in seq_along(r$values)) {
  if (r$values[i]) {
    intervals <- rbind(
      intervals,
      data.frame(
        start = dates[starts[i]],
        end = dates[ends[i]]
      )
    )
  }
}

# ----------------------------
# NORMALIZE TIMEZONE SAFELY
# ----------------------------

# exact UTC timestamps (canonical)
intervals$start_datetime_utc <- format(
  intervals$start,
  tz = "UTC",
  usetz = TRUE
)

intervals$end_datetime_utc <- format(
  intervals$end,
  tz = "UTC",
  usetz = TRUE
)

# LA-local display dates
intervals$start_date_la <- as.Date(
  format(intervals$start, tz = display_tz)
)

intervals$end_date_la <- as.Date(
  format(intervals$end, tz = display_tz)
)

# ----------------------------
# ADD METADATA
# ----------------------------

intervals$year <- as.numeric(format(intervals$start_date, "%Y"))
intervals$duration_days <- as.numeric(intervals$end_date - intervals$start_date)
intervals$cycle_index <- seq_len(nrow(intervals))

same_month <- format(intervals$start_date_la, "%Y-%m") ==
  format(intervals$end_date_la, "%Y-%m")

intervals$dates_la <- ifelse(
  same_month,
  paste0(
    format(intervals$start_date_la, "%b "),
    as.integer(format(intervals$start_date_la, "%d")),
    "–",
    as.integer(format(intervals$end_date_la, "%d"))
  ),
  paste0(
    format(intervals$start_date_la, "%b %d"),
    " - ",
    format(intervals$end_date_la, "%b %d")
  )
)

# ----------------------------
# FINAL CLEAN OUTPUT
# ----------------------------

final_df <- intervals[, c(
  "start_datetime_utc",
  "end_datetime_utc",
  "start_date_la",
  "end_date_la",
  "dates_la",
  "year",
  "duration_days",
  "cycle_index"
)]

# ----------------------------
# WRITE CSV
# ----------------------------

write.csv(
  final_df,
  "Data/mercury_retrograde_2000_2100.csv",
  row.names = FALSE
)

# ----------------------------
# DONE
# ----------------------------
