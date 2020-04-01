## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      eval = FALSE, 
                      fig.align = "center")

## ----message=FALSE, warning=FALSE, include=FALSE, eval = TRUE-----------------
library(lubridate)
library(scales)
library(ggplot2)
library(dplyr)
library(hystReet)

## -----------------------------------------------------------------------------
#  Sys.setenv(HYSTREET_API_TOKEN = "PASTE YOUR API TOKEN HERE")

## -----------------------------------------------------------------------------
#  library(hystReet)
#  
#  stats <- get_hystreet_stats()

## -----------------------------------------------------------------------------
#  locations <- get_hystreet_locations()

## ---- eval = TRUE, echo=FALSE-------------------------------------------------
knitr::kable(
  locations[1:10,],
  format = "html"
)

## -----------------------------------------------------------------------------
#  location_71 <- get_hystreet_station_data(
#    hystreetId = 71,
#    query = list(from = "2018-12-01", to = "2018-12-31", resolution = "day"))

## -----------------------------------------------------------------------------
#  location_71 <- get_hystreet_station_data(
#      hystreetId = 71,
#      query = list(from = "2018-12-01", to = "2019-01-01", resolution = "hour"))

## ---- eval = TRUE-------------------------------------------------------------
ggplot(location_71$measurements, aes(x = timestamp, y = pedestrians_count, colour = weekdays(timestamp))) +
  geom_path(group = 1) +
  scale_x_datetime(date_breaks = "7 days") +
  scale_x_datetime(labels = date_format("%d.%m.%Y")) +
  labs(x = "Date",
       y = "Pedestrians",
       colour = "Day")

## -----------------------------------------------------------------------------
#  location_73 <- get_hystreet_station_data(
#      hystreetId = 73,
#      query = list(from = "2019-01-01", to = "2019-01-31", resolution = "day"))$measurements %>%
#    select(pedestrians_count, timestamp) %>%
#    mutate(station = 73)
#  
#  location_74 <- get_hystreet_station_data(
#      hystreetId = 74,
#      query = list(from = "2019-01-01", to = "2019-01-31", resolution = "day"))$measurements %>%
#      select(pedestrians_count, timestamp) %>%
#    mutate(station = 74)
#  
#  data <- bind_rows(location_73, location_74)

## ----eval =TRUE---------------------------------------------------------------
ggplot(data, aes(x = timestamp, y = pedestrians_count, fill = weekdays(timestamp))) +
  geom_bar(stat = "identity") +
  scale_x_datetime(labels = date_format("%d.%m.%Y")) +
  facet_wrap(~station, scales = "free_y") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

## ----message=FALSE, warning=FALSE---------------------------------------------
#  hystreet_ids <- get_hystreet_locations()
#  
#  all_data <- lapply(hystreet_ids[,"id"], function(x){
#    temp <- get_hystreet_station_data(
#      hystreetId = x)
#  
#  
#      lifetime_count <- temp$statistics$lifetime_count
#      days_counted <- as.numeric(temp$metadata$latest_measurement_at  - temp$metadata$earliest_measurement_at)
#  
#      return(data.frame(
#        id = x,
#        station = paste0(temp$city, " (",temp$name,")"),
#        ratio = lifetime_count/days_counted))
#  
#  })
#  
#  ratio <- bind_rows(all_data)

## ---- eval = TRUE-------------------------------------------------------------
ratio %>% 
  top_n(5, ratio) %>% 
  arrange(desc(ratio))

## ---- eval = TRUE-------------------------------------------------------------
ggplot(ratio %>% 
         top_n(10,ratio), aes(station, ratio)) +
  geom_bar(stat = "identity") +
  labs(x = "City",
       y = "Pedestrians per day") + 
    theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

