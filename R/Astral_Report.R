library(dplyr)
library(lubridate)
library(readr)
library(glue)

# =========================================
# DATA
# =========================================

year <- year(now(tzone = "America/Los_Angeles"))
today <- as_date(Sys.time(), tz = "America/Los_Angeles")

moon_data <- read_csv("Data/moon_calendar_2000_2100.csv") |>
  mutate(illumination_pct = round(illumination_pct, 0))

season_data <- read_csv("Data/seasonal_events_2000_2100.csv")
mercury_data <- read_csv("Data/mercury_retrograde_2000_2100.csv")
meteor_data <- read_csv("Data/meteor_showers_2000_2100.csv")
eclipse_data <- read_csv("Data/solar_eclipses_2000_2100.csv")

# =========================================
# MOON
# =========================================

build_moon <- function(moon_data, today) {
  moon_today <- moon_data |>
    filter(date == today)

  if (nrow(moon_today) == 0) {
    return("Lunar data unavailable")
  }

  next_new_moon <- moon_data |>
    filter(date > today, is_new_moon == TRUE) |>
    slice(1)

  moon_filename <- glue(
    "moon_phase_{moon_today$moon_phase[1]}_{moon_today$illumination_pct[1]}.png"
  )

  moon_url <- glue(
    "https://raw.githubusercontent.com/Josephhero/astral_events/main/Images/{moon_filename}"
  )

  moon_text <- glue(
    "<b>Lunar Illumination:</b> {moon_today$illumination_pct}% ",
    "<img src='{moon_url}' alt='Moon' style='width:30px;height:30px;vertical-align:middle;'><br>",
    "Age of Moon: {moon_today$moon_age_days} Days<br>",
    "Next New Moon: {format(next_new_moon$date, '%b %d, %Y')} ({moon_today$days_until_new_moon} days)"
  )

  event_text <- ""

  if (isTRUE(moon_today$is_new_moon)) {
    event_text <- glue(
      "New Moon<br>",
      "The Moon is currently at the beginning of a new lunar cycle."
    )
  } else if (isTRUE(moon_today$is_full_moon)) {
    if (isTRUE(moon_today$is_blue_moon)) {
      event_text <- glue(
        "Blue Moon<br>",
        "{moon_today$special_moon_desc}"
      )
    } else {
      event_text <- glue(
        "{moon_today$named_moon}<br>",
        "{moon_today$named_moon_desc}"
      )

      if (isTRUE(moon_today$is_harvest_moon)) {
        event_text <- glue(
          "{event_text}<br>",
          "It is also the Harvest Moon.<br>",
          "{moon_today$special_moon_desc}"
        )
      }
    }
  }

  glue(
    "{moon_text}",
    if (event_text != "") glue("<br>{event_text}") else ""
  )
}

# =========================================
# SEASONAL EVENTS
# =========================================

build_season <- function(season_data, today) {
  today_events <- season_data |>
    filter(event_la_date == today)

  if (nrow(today_events) > 0) {
    return(glue(
      "<b>Season Change Today:</b><br>",
      "{today_events$event}<br>",
      "{today_events$event_desc}"
    ))
  }

  next_event <- season_data |>
    filter(event_la_date > today) |>
    arrange(event_la_date) |>
    slice(1)

  glue(
    "<b>Next Season Change:</b> {format(next_event$event_la_date, '%b %d, %Y')}<br>",
    "{next_event$event}<br>",
    "{next_event$event_desc}"
  )
}

# =========================================
# MERCURY
# =========================================

build_mercury <- function(mercury_data, today) {
  active <- mercury_data |>
    filter(today >= start_date_la & today <= end_date_la)

  if (nrow(active) > 0) {
    days_remaining <- as.integer(active$end_date_la - today)

    return(glue(
      "<b>Mercury in Retrograde</b><br>",
      "Dates: {active$dates_la}<br>",
      "Days Remaining: {days_remaining}"
    ))
  }

  next_retro <- mercury_data |>
    filter(start_date_la > today) |>
    arrange(start_date_la) |>
    slice(1)

  glue(
    "<b>Mercury Not in Retrograde</b><br>",
    "Next Retrograde Event: {next_retro$dates_la}"
  )
}

# =========================================
# ECLIPSE
# =========================================

build_eclipse <- function(eclipse_data, today) {
  eclipse_today <- eclipse_data |>
    filter(date_la == today)

  if (nrow(eclipse_today) > 0) {
    duration <- if (!is.na(eclipse_today$central_duration_text)) {
      glue("<br>Duration: {eclipse_today$central_duration_text}")
    } else {
      ""
    }

    return(glue(
      "<b>Eclipse Today</b><br>",
      "{eclipse_today$eclipse_type_long} Solar Eclipse<br>",
      "--{eclipse_today$eclipse_desc}<br>",
      "Visibility: {eclipse_today$eclipse_visibility_ca}",
      "{duration}"
    ))
  }

  next_eclipse <- eclipse_data |>
    filter(date_la > today) |>
    arrange(date_la) |>
    slice(1)

  duration <- if (!is.na(next_eclipse$central_duration_text)) {
    glue("<br>Duration: {next_eclipse$central_duration_text}")
  } else {
    ""
  }

  glue(
    "<b>Next Eclipse:</b> {format(next_eclipse$date_la, '%b %d, %Y')}<br>",
    "{next_eclipse$eclipse_type_long} Solar Eclipse<br>",
    "--{next_eclipse$eclipse_desc}<br>",
    "Visibility: {next_eclipse$eclipse_visibility_ca}",
    "{duration}"
  )
}

# =========================================
# METEOR SHOWERS
# =========================================

build_meteor <- function(meteor_data, today) {
  active <- meteor_data |>
    filter(today >= active_start & today <= active_end) |>
    slice(1)

  if (nrow(active) > 0) {
    return(glue(
      "<b>Meteor Shower Active:</b> {active$shower_name}<br>",
      "Active: {active$active_date_label}<br>",
      "Peak Date: {format(active$peak_date, '%b %d')}<br>",
      "Activity Rate: {active$intensity}<br>",
      "Peak Activity: {active$zhr} meteors/hr"
    ))
  }

  next_shower <- meteor_data |>
    filter(active_start > today) |>
    arrange(active_start) |>
    slice(1)

  glue(
    "<b>Next Meteor Shower:</b> {next_shower$shower_name}<br>",
    "Active: {next_shower$active_date_label}<br>",
    "Peak Date: {format(next_shower$peak_date, '%b %d')}<br>",
    "Activity Rate: {next_shower$intensity}<br>",
    "Peak Activity: {next_shower$zhr} meteors/hr"
  )
}

# =========================================
# BUILD SECTIONS
# =========================================

sections <- list(
  build_moon(moon_data, today),
  build_season(season_data, today),
  build_mercury(mercury_data, today),
  build_meteor(meteor_data, today),
  build_eclipse(eclipse_data, today)
)

sections <- sections[!sapply(sections, is.null)]

message <- paste(sections, collapse = "<br><br>")

# =========================================
# SUBJECT
# =========================================

subject <- glue(
  "Astral Report: {format(today, '%B %d, %Y')}"
)

# =========================================
# OUTPUT
# =========================================

writeLines(subject, "email_subject.txt")
writeLines(message, "email_body.txt")

# This part is only for debugging in RStudio.
# You do not need it for GitHub Actions

# library(gmailr)
#
# astral_email <-
#   gm_mime() |>
#   gm_to("mmrojas1986@gmail.com") |>
#   gm_from("hefnerjoseph87@gmail.com") |>
#   gm_subject(subject) |>
#   gm_html_body(message)
#
# gm_send_message(astral_email)
