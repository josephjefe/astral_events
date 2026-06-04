library(swephR)


get_sun_lon <- function(dt) {
  jd <- swephR::swe_julday(
    as.numeric(format(dt, "%Y")),
    as.numeric(format(dt, "%m")),
    as.numeric(format(dt, "%d")),
    as.numeric(format(dt, "%H")) +
      as.numeric(format(dt, "%M")) / 60,
    1
  )

  sun <- swephR::swe_calc_ut(jd, 0, 2)

  sun$xx[1]
}

find_event_day <- function(year, target_angle) {
  days <- seq(
    as.Date(paste0(year, "-01-01")),
    as.Date(paste0(year, "-12-31")),
    by = "day"
  )

  diffs <- sapply(days, function(d) {
    dt <- as.POSIXct(paste(d, "12:00:00"), tz = "UTC")

    lon <- get_sun_lon(dt)

    abs(((lon - target_angle + 180) %% 360) - 180)
  })

  days[which.min(diffs)]
}

refine_event_time <- function(day, target_angle) {
  times <- seq(
    as.POSIXct(paste(day, "00:00:00"), tz = "UTC"),
    as.POSIXct(paste(day, "23:00:00"), tz = "UTC"),
    by = "1 hour"
  )

  diffs <- sapply(times, function(t) {
    lon <- get_sun_lon(t)

    abs(((lon - target_angle + 180) %% 360) - 180)
  })

  times[which.min(diffs)]
}

find_season_event <- function(year, target_angle) {
  # STEP 1: find best day
  approx_day <- find_event_day(year, target_angle)

  # STEP 2: refine to actual UTC time
  event_utc <- refine_event_time(approx_day, target_angle)

  data.frame(
    year = year,
    event_utc = event_utc
  )
}

get_seasonal_events <- function(year) {
  events <- data.frame(
    event = c(
      "Spring Equinox",
      "Summer Solstice",
      "Autumn Equinox",
      "Winter Solstice"
    ),
    angle = c(0, 90, 180, 270)
  )

  do.call(
    rbind,
    lapply(1:nrow(events), function(i) {
      res <- find_season_event(year, events$angle[i])

      data.frame(
        event = events$event[i],
        event_utc = res$event_utc
      )
    })
  )
}

years <- 2000:2100

seasonal_data <- dplyr::bind_rows(
  lapply(years, function(y) {
    get_seasonal_events(y) |>
      dplyr::mutate(year = y)
  })
)

seasonal_data_new <- seasonal_data |>
  mutate(event_la = lubridate::with_tz(event_utc, "America/Los_Angeles")) |>
  mutate(event_la_date = lubridate::date(event_la)) |>
  mutate(
    event_desc = case_when(
      event %in%
        c(
          "Spring Equinox",
          "Autumn Equinox"
        ) ~ "Day and night are approximately equal.",
      event == "Summer Solstice" ~ "The longest day of the year.",
      event == "Winter Solstice" ~ "The longest night of the year."
    )
  )

write_csv(seasonal_data_new, "Data/seasonal_events_2000_2100.csv")
