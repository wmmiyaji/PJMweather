# Weather Load Forecasts 
library(RCurl)
library(jsonlite)
library(tidyverse)
library(purrrlyr)
library(lubridate)
#Reference: https://forecast-v3.weather.gov/documentation?redirect=legacy 
tzNY <- "America/New_York"

station_table <- station_table %>% 
  mutate(TZ = ifelse(Station == "ORD", "America/Chicago", "America/New_York"))

get_forecast <- function(X){
  # X is a vector with three elements lat, lon and icao ( four character station code)
  # intended to be used with by_row from purrrlyr
  latitude <- X$lat
  longitude <- X$lon
  four_character_station <-X$icao
  weather_url_left <- "https://api.weather.gov"
  points_url <- paste0(weather_url_left, "/points/",latitude,",",longitude)
  json_weather_points <- getURL(points_url,
                                httpheader = c('User-Agent' = "Hello, World"))
  weather_points <- fromJSON(json_weather_points)
  hourly_url <- weather_points$properties$forecastHourly
  json_hourly_forecast <- getURL(hourly_url,
                                 httpheader = c('User-Agent' = "Hello, World"))
  weather_hourly_forecast <- fromJSON(json_hourly_forecast)
  forecast_df <- weather_hourly_forecast$properties$periods %>% 
    mutate(station = four_character_station)
}

get_update_time <- function(X){
  # X is a vector with three elements lat, lon and icao ( four character station code)
  # intended to be used with by_row from purrrlyr
  latitude <- X$lat
  longitude <- X$lon
  four_character_station <-X$icao
  weather_url_left <- "https://api.weather.gov"
  points_url <- paste0(weather_url_left, "/points/",latitude,",",longitude)
  json_weather_points <- getURL(points_url,
                                httpheader = c('User-Agent' = "Hello, World"))
  weather_points <- fromJSON(json_weather_points)
  hourly_url <- weather_points$properties$forecastHourly
  json_hourly_forecast <- getURL(hourly_url,
                                 httpheader = c('User-Agent' = "Hello, World"))
  weather_hourly_forecast <- fromJSON(json_hourly_forecast)
  update_time <- weather_hourly_forecast$properties$updateTime
}








#save(file="otherwise.RData", otherwise_df)
load(file="otherwise.RData")
possibly_get_forecast <- possibly(get_forecast,  otherwise = otherwise_df)  
# for otherwise_df all the values have been set to NA 
# otherwise_df <- forecast_df[1,]   where forecast_df was a good output 
# otherwise_df[1,1:ncol(otherwise_df)] <- NA
# this is kludgy, I am open to suggestions 
possibly_get_update_time <- possibly(get_update_time, otherwise = NA) 

all_station_forecasts <- stations_list  %>% by_row(..f=function(X){
  update_time <- possibly_get_update_time(X)
  possibly_get_forecast(X) %>% 
    mutate(update_time = update_time)
}) %>% unnest()  


UPDATE_TIME <- max(unique(all_station_forecasts$update_time))

# kludge for dealing with the time zones. Only KORD is central time so we split the forecasts, fix the tz and bind back together 
# this can then be assembled by PJM zone weighting
CT_forecasts <- all_station_forecasts %>% select(icao, startTime, temperature)  %>% filter(icao == "KORD") %>% 
  mutate(local_time = ymd_hms(startTime, tz="America/Chicago"))
ET_forecasts <- all_station_forecasts %>% select(icao, startTime, temperature)  %>% filter(icao != "KORD") %>% 
  mutate(local_time = ymd_hms(startTime, tz="America/New_York"))

all_station_forecasts_abb <- rbind(CT_forecasts, ET_forecasts)

# 2018-04-02T15:00:00-04:00  this is April 2, 2018 3:00 PM Eastern Daylight Time 
# ymd_hms("2018-04-02T15:00:00-04:00", tz = "America/New_York")

#TODO: deal with missing data 
# case of some of the forecasts are bad 
   # reuse the time from the first good station 
# case all of the forecasts are good 
   # no problem 

# Get the zone forecast by combining these results and the table from M19


#ZONE <- unique(station_table$Zone)[2]
#ZONE <- "PEPCO"
all_zone_forecast <- unique(station_table$Zone) %>% 
  map_df(.f=function(ZONE){
    
   one_zone <- station_table %>% filter(Zone == ZONE) %>% 
     mutate(Station = paste0("K", Station), 
            Weight = as.numeric(Weight)) %>% 
     select(Station, Weight, Airport_Name) 

# TODO: this will break if data is missing 
  one_zone_forecast <- all_station_forecasts_abb  %>%  
    filter(icao %in% one_zone$Station) %>% left_join(one_zone, by=c("icao"="Station")) %>% 
    select(local_time, icao, Airport_Name,  temperature, Weight) %>% 
    mutate(temperature_weighted = temperature * Weight) %>% 
    group_by(local_time) %>% 
    dplyr::summarize(Wt_Temperature = sum(temperature_weighted, na.rm=T)) %>% 
    mutate(Zone = ZONE)
  one_zone_forecast 
})

pdf(paste0("zone_temperature_forecasts",UPDATE_TIME,".pdf"), width=11, height=8.5)

g_1 <- all_zone_forecast %>% 
  ggplot(aes(x=local_time, y=Wt_Temperature)) + 
  geom_line() + 
  facet_wrap(~Zone) + 
  labs(x="Date Time", y="Temperature (F)", 
       title =paste0("Weighted Temperture Forecast by PJM\n
         weighting by Manual 19, Exhibit 2\n
         Source=https://api.weather.gov",
         "\nEval Time ", UPDATE_TIME)) +
  theme_bw() 

print(g_1)

unique(all_zone_forecast$Zone) %>% walk(.f=function(ZONE){
  g_2 <- all_zone_forecast %>% filter(Zone == ZONE) %>% 
    ggplot(aes(x=local_time, y=Wt_Temperature)) + 
    geom_line() + 
    labs(x="Date Time", y="Temperature (F)", 
         title =paste0("Weighted Temperture Forecast for Zone ",ZONE,
         "\nweighting by Manual 19, Exhibit 2",
         "\nSource=https://api.weather.gov",
         "\nEval Time ", UPDATE_TIME)) +
    theme_bw() 
  print(g_2)
})

dev.off()   


