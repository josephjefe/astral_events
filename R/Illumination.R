library(swephR)
library(dplyr)

# =========================================
# CONFIG
# =========================================

tz <- "America/Los_Angeles"

start_date <- as.Date("2000-01-01")
end_date <- as.Date("2100-12-31")

dates <- seq(start_date, end_date, by = "day")

# =========================================
# MAIN FUNCTION
# =========================================

get_moon_data <- function(date, tz = "America/Los_Angeles") {
  dt_la <- as.POSIXct(
    paste(date, "21:00:00"),
    tz = tz
  )

  dt_utc <- as.POSIXct(
    format(dt_la, tz = "UTC"),
    tz = "UTC"
  )

  year_utc <- as.numeric(format(dt_utc, "%Y"))
  month_utc <- as.numeric(format(dt_utc, "%m"))
  day_utc <- as.numeric(format(dt_utc, "%d"))

  hour_utc <-
    as.numeric(format(dt_utc, "%H")) +
    as.numeric(format(dt_utc, "%M")) / 60

  jd <- swephR::swe_julday(
    year_utc,
    month_utc,
    day_utc,
    hour_utc,
    1
  )

  ph <- swephR::swe_pheno_ut(
    jd,
    1,
    2
  )

  attr <- ph$attr

  illumination_raw <- attr[2]

  illumination <- ifelse(
    illumination_raw > 1,
    illumination_raw / 100,
    illumination_raw
  )

  data.frame(
    date = as.Date(date),

    datetime_la = dt_la,
    datetime_utc = dt_utc,

    illumination_pct = round(illumination * 100, 1)
  )
}

# =========================================
# BUILD DAILY DATA
# =========================================

moon_data1 <- bind_rows(
  lapply(dates, get_moon_data)
)

# =========================================
# DETECT FULL / NEW MOONS
# =========================================

moon_data2 <- moon_data1 |>
  arrange(date) |>
  mutate(
    lag_illum = lag(illumination_pct),
    lead_illum = lead(illumination_pct),

    is_full_moon = illumination_pct > lag_illum &
      illumination_pct >= lead_illum,

    is_new_moon = illumination_pct < lag_illum &
      illumination_pct <= lead_illum
  )


# =========================================
# MOON AGE
# =========================================

moon_data3 <- moon_data2 |>
  mutate(
    moon_number = cumsum(is_new_moon)
  ) |>
  group_by(moon_number) |>
  mutate(
    moon_age_days = row_number() - 1,
    cycle_length = n()
  ) |>
  ungroup() |>
  mutate(
    days_until_new_moon = cycle_length -
      moon_age_days -
      1
  )

# =========================================
# NAMED FULL MOONS
# =========================================

full_moons <- moon_data3 |>
  filter(is_full_moon) |>
  mutate(
    named_moon = case_when(
      format(date, "%m") == "01" ~ "Wolf Moon",
      format(date, "%m") == "02" ~ "Snow Moon",
      format(date, "%m") == "03" ~ "Worm Moon",
      format(date, "%m") == "04" ~ "Pink Moon",
      format(date, "%m") == "05" ~ "Flower Moon",
      format(date, "%m") == "06" ~ "Strawberry Moon",
      format(date, "%m") == "07" ~ "Buck Moon",
      format(date, "%m") == "08" ~ "Sturgeon Moon",
      format(date, "%m") == "09" ~ "Corn Moon",
      format(date, "%m") == "10" ~ "Hunter's Moon",
      format(date, "%m") == "11" ~ "Beaver Moon",
      format(date, "%m") == "12" ~ "Cold Moon"
    ),

    year_month = format(date, "%Y-%m")
  ) |>
  group_by(year_month) |>
  mutate(
    full_moon_number = row_number(),
    is_blue_moon = full_moon_number == 2
  ) |>
  ungroup()

# =========================================
# HARVEST MOON
# =========================================

harvest_moons <- full_moons |>
  mutate(
    year = as.integer(format(date, "%Y")),
    autumn_equinox = as.Date(paste0(year, "-09-22")),
    equinox_diff = abs(as.numeric(date - autumn_equinox))
  ) |>
  group_by(year) |>
  slice_min(
    equinox_diff,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  transmute(
    date,
    is_harvest_moon = TRUE
  )

# =========================================
# EVENT METADATA
# =========================================

moon_event_data <- moon_data3 |>
  select(
    date,
    is_full_moon,
    is_new_moon
  ) |>
  left_join(
    full_moons |>
      select(
        date,
        named_moon,
        is_blue_moon
      ),
    by = "date"
  ) |>
  left_join(
    harvest_moons,
    by = "date"
  ) |>
  mutate(
    is_blue_moon = coalesce(is_blue_moon, FALSE),

    is_harvest_moon = coalesce(is_harvest_moon, FALSE),

    special_moon = case_when(
      is_blue_moon ~ "Blue Moon",
      is_harvest_moon ~ "Harvest Moon",
      TRUE ~ NA_character_
    )
  )

# =========================================
# FINAL DATASET
# =========================================

moon_data <- moon_data3 |>
  left_join(
    select(
      moon_event_data,
      date,
      named_moon,
      is_blue_moon,
      is_harvest_moon,
      special_moon
    ),
    by = "date"
  ) |>
  left_join(readr::read_csv("Data/Moon Names.csv"), by = "named_moon") |>
  left_join(
    select(
      readr::read_csv("Data/Moon Names.csv"),
      special_moon = named_moon,
      special_moon_desc = named_moon_desc
    ),
    by = "special_moon"
  ) |>
  mutate(
    next_illumination = lead(illumination_pct),

    moon_phase = case_when(
      next_illumination > illumination_pct ~ "waxing",
      TRUE ~ "waning"
    )
  ) |>
  select(
    date,
    datetime_la,
    datetime_utc,

    illumination_pct,

    moon_number,
    moon_age_days,
    days_until_new_moon,

    named_moon,
    special_moon,
    named_moon_desc,
    special_moon_desc,
    moon_phase,

    is_full_moon,
    is_new_moon,
    is_blue_moon,
    is_harvest_moon
  )

# =========================================
# WRITE CSV
# =========================================

write.csv(
  moon_data,
  "Data/moon_calendar_2000_2100.csv",
  row.names = FALSE
)
