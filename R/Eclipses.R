library(readr)
library(janitor)


eclipses_raw <- read_csv("Data/solar_eclipses_2001_2100.csv")

# California latitude and longitude
ca_lat <- 36.7783
ca_lon <- -119.4179

# Haversine function
haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180

  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad

  a <-
    sin(dlat / 2)^2 +
    cos(lat1 * rad) *
      cos(lat2 * rad) *
      sin(dlon / 2)^2

  6371 *
    2 *
    atan2(
      sqrt(a),
      sqrt(1 - a)
    )
}

format_duration <- function(seconds) {
  if (is.na(seconds)) {
    return(NA_character_)
  }

  minutes <- seconds %/% 60
  secs <- seconds %% 60

  paste0(minutes, "m ", secs, "s")
}

eclipses <- eclipses_raw |>
  janitor::clean_names() |>
  dplyr::mutate(
    eclipse_type_long = dplyr::case_when(
      substr(eclipse_type, 1, 1) == "P" ~ "Partial",
      substr(eclipse_type, 1, 1) == "A" ~ "Annular",
      substr(eclipse_type, 1, 1) == "T" ~ "Total",
      substr(eclipse_type, 1, 1) == "H" ~ "Hybrid",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::mutate(
    eclipse_desc = dplyr::case_when(
      eclipse_type_long == "Partial" ~
        "The Moon partially covers the Sun.",

      eclipse_type_long == "Annular" ~
        "The Moon appears smaller than the Sun, creating a ring of light.",

      eclipse_type_long == "Total" ~
        "The Moon completely covers the Sun.",

      eclipse_type_long == "Hybrid" ~
        "A rare eclipse that appears total in some locations and annular in others.",

      TRUE ~ NA_character_
    )
  ) |>
  dplyr::mutate(
    distance_to_ca_km = haversine_km(
      latitude,
      longitude,
      ca_lat,
      ca_lon
    ),

    eclipse_visibility_ca = dplyr::case_when(
      # central path reasonably near CA
      eclipse_type_long %in%
        c(
          "Total",
          "Annular",
          "Hybrid"
        ) &
        !is.na(path_width_km) &
        distance_to_ca_km <=
          (path_width_km / 2 + 400) ~ "Fully Visible in California",

      # broad partial visibility
      eclipse_type_long == "Partial" &
        distance_to_ca_km <= 7000 ~ "Partially Visible in California",

      eclipse_type_long %in%
        c(
          "Total",
          "Annular",
          "Hybrid"
        ) &
        distance_to_ca_km <= 5000 ~ "Partially Visible in California",

      TRUE ~ "Not Visible in California"
    )
  ) |>
  dplyr::mutate(
    central_duration_text = sapply(central_duration_s, format_duration)
  )

write_csv(eclipses, "Data/solar_eclipses_2000_2100.csv")
