
install.packages("rnoaa")
library(rnoaa)  # seems like you no longer need the API key but maybe it is stored somewhere on my PC anyway 
library(tidyverse)
install.packages("isdparser")
library(isdparser)
library(stringr)
library(lubridate)
year.list <- 2014:2018 
station_table <- read.csv(paste0(HOMEDIR, "/station_table.csv"))
pjm_station_list <- paste0("K",unique(station_table$Station)) 

stations_list <- isd_stations() %>% filter(icao %in% pjm_station_list) %>% 
  group_by(icao) %>% top_n(1,end) %>% ungroup()   

# this gets the USAF and WBAN identifiers for the most recent instance of the airport code
# seems like these USAF, WBAN pairs are good back to 2001 for these stations
# If you want more that you need to be a little more clever
# isd() is basically a download of a file from a directory so you need a YEAR, USAF, WBAN to get anything 

# This takes about 6 hours for the 28 stations and 5 years the first time
# isd() caches the annual data by station on your PC and then it is much faster the next time 
# So don't download stuff you will never need (or cleanup!)

# Gets a data.frame of temperature, dewpoint for the PJM specified airport 
# stations with UTC and America/New_York times

pjm_weather <- stations_list$icao %>%     
  map_df(.f = function(STATION) {
    
    USAF <- (stations_list %>% filter(icao == STATION))$usaf[1]
    WBAN <- (stations_list %>% filter(icao == STATION))$wban[1]
    
    one.station.weather <- year.list %>% map_df(.f = function(YEAR)  {
      
      one.year <- isd(usaf = USAF, wban = WBAN, year = YEAR) %>% 
        isd_transform() %>% 
        select(date, time, latitude, longitude, wind_direction, temperature,
               temperature_dewpoint, air_pressure, visibility_distance) %>% 
        ungroup() %>% 
        mutate(Hour_UTC = substr(time,1,2),
               Minute  = substr(time, 3,4),  # Minute is DST invariant 
               Date_Time_UTC = ymd_hm(paste(date, Hour_UTC, 0 ), tz = "UTC"),
               Date_Time_NY = with_tz(Date_Time_UTC, tz = "America/New_York"),
               Date_Only_NY = format(Date_Time_NY, "%Y-%m-%d"), 
               Hour_NY = hour(Date_Time_NY),
               icao = STATION)
      
      # trying to be careful here to handle DST correctly for when you join to meter data and PJM data
      # NOTE:  that hour is being rounded down, good for when the temperature is joined to an "hour beginning" 
      # for the meter data, LMP, etc.  
      
      return(one.year)
    })
    
    # to get one measurement per hour find the minute the is most common and then filter on that 

    
    Minute_to_keep <- as.character(one.station.weather %>% group_by(Minute) %>% 
                                     dplyr::summarize(Count = n()) %>% 
                                     arrange(desc(Count)) %>% 
                                     head(1) %>%
                                     select(Minute)
                                   )
    
    one.station.weather <- one.station.weather %>% 
      filter(Minute == Minute_to_keep)
    
    return(one.station.weather)
  })

# Next step here is to get the weighted series for each Zone from the weather station data 
# Then get all the functions from the manual, e.g. WTHI 

