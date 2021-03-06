# PJM Historic LMP
library(RCurl)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(stringr)
# my standard use date_time_utc for the posix ct time with TZ for the datetime_beginning_utc from the PJM API
# then extract Year_utc, Month_utc, Day_utc, Hour_utc to do the joins 

pad_2 <- function(X) {str_pad(X, 2, pad = "0")}  # so you can write 03 rather than 3 in 2018-03-01 
DATE <- as.Date("2018-03-01")
source("PJM_creds.R")  # not in github, one line,  "PJM_API_KEY <- "Ocp-Apim-Subscription-Key", get your own!
tzNY <- "America/New_York"
tzCH <- "America/Chicago" 
# key reference: http://www.pjm.com/-/media/etools/data-miner-2/data-miner-2-api-guide.ashx?la=en
# A pnode_id is 51301 for PSE&G one can test with 

get_da_hrl_lmps <- function(START_DATE_EPT, END_DATE_EPT, PNODE_ID) {
  seq(as.Date(START_DATE_EPT), as.Date(END_DATE_EPT), by="days") %>% 
    map_df(.f=function(DATE){
      DATE_GET <- paste0(year(DATE), "/", pad_2(month(DATE)), "-", pad_2(day(DATE)))
      da_hrl_lmps_url <- paste0("https://api.pjm.com/api/v1/da_hrl_lmps?rowCount=5000&startRow=1&datetime_beginning_ept=",DATE_GET,"&pnode_id=",PNODE_ID)
      da_hrl_lmps <- getURL(da_hrl_lmps_url , httpheader = c(paste0("Ocp-Apim-Subscription-Key: ", PJM_API_KEY)))
      da_hrl_lmps <- fromJSON(da_hrl_lmps)
      da_hrl_lmps_df<- da_hrl_lmps$items
    }) %>% 
    mutate(date_time_utc = ymd_hms(datetime_beginning_utc), 
           year_utc = year(date_time_utc), 
           month_utc = month(date_time_utc), 
           day_utc = day(date_time_utc), 
           hour_utc = hour(date_time_utc)
    )
}
         





