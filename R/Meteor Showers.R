library(dplyr)
library(tidyr)

# Data-----

meteor_templates <- tibble::tribble(
  ~shower_name       , ~start_mmdd , ~peak_mmdd , ~end_mmdd , ~zhr ,
  "Quadrantids"      , "01-01"     , "01-03"    , "01-05"   ,  120 ,
  "Lyrids"           , "04-15"     , "04-22"    , "04-29"   ,   18 ,
  "Eta Aquariids"    , "04-19"     , "05-06"    , "05-28"   ,   50 ,
  "Delta Aquariids"  , "07-12"     , "07-30"    , "08-23"   ,   25 ,
  "Perseids"         , "07-17"     , "08-12"    , "08-24"   ,  100 ,
  "Draconids"        , "10-06"     , "10-08"    , "10-10"   ,   10 ,
  "Orionids"         , "10-02"     , "10-22"    , "11-07"   ,   20 ,
  "Southern Taurids" , "09-23"     , "10-10"    , "11-27"   ,    5 ,
  "Northern Taurids" , "10-13"     , "11-12"    , "12-02"   ,    5 ,
  "Leonids"          , "11-06"     , "11-17"    , "11-30"   ,   15 ,
  "Geminids"         , "12-04"     , "12-14"    , "12-20"   ,  150 ,
  "Ursids"           , "12-17"     , "12-22"    , "12-26"   ,   10
)

years <- 2000:2100

meteor_showers <- tidyr::crossing(
  year = years,
  meteor_templates
) |>
  mutate(
    active_start = as.Date(
      paste(year, start_mmdd, sep = "-")
    ),

    peak_date = as.Date(
      paste(year, peak_mmdd, sep = "-")
    ),

    active_end = as.Date(
      paste(year, end_mmdd, sep = "-")
    ),

    intensity = case_when(
      zhr >= 120 ~ "Very High",
      zhr >= 80 ~ "High",
      zhr >= 40 ~ "Moderate",
      zhr >= 15 ~ "Low",
      TRUE ~ "Very Low"
    ),

    peak_date_label = format(
      peak_date,
      "%b %d"
    ),

    active_date_label = paste0(
      format(active_start, "%b %d"),
      " - ",
      format(active_end, "%b %d")
    )
  ) |>
  select(
    year,
    shower_name,
    active_start,
    peak_date,
    active_end,
    active_date_label,
    peak_date_label,
    zhr,
    intensity
  )

write.csv(meteor_showers, "Data/meteor_showers_2000_2100.csv")
