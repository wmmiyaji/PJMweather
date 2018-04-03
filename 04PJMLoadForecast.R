## PJM LOAD FORECAST 

library(RCurl)
library(jsonlite)
library(tidyverse)
library(lubridate)
source("PJM_creds.R")  # not in github, one line,  "PJM_API_KEY <- "Ocp-Apim-Subscription-Key", get your own!
tzNY = "America/New_York"


forecast_area_list <- c("AE/MIDATL", "AEP", "AP", "ATSI", "BG&E/MIDATL", "COMED", "DAYTON", "DEOK", "DOMINION", 
                        "DP&L/MIDATL", "DUQUESNE", "EKPC", "JCP&L/MIDATL", "METED/MIDATL", "MID_ATLANTIC_REGION", 
                        "PECO/MIDATL", "PENELEC/MIDATL", "PEPCO/MIDATL", "PPL/MIDATL", "PSE&G/MIDATL", "RECO", 
                        "RTO_COMBINED", "SOUTHERN_REGION", "UGI/MIDATL", "WESTERN_REGION")
zone_list <- c( "AECO", "AEP", "APS", "ATSI", "BGE", "COMED", "CPL", "DAY", "DEOK", "DOM", "DPL", "DUKE", 
                "DUQ", "EKPC", "EXTERNAL", "JCPL", "METED", "PECO", "PENELEC", "PEPCO", "PPL", "PSEG", "RECO")
# think about a zone to forecast area crosswalk 

# forecast for one area is 168 records
# 25 different areas are available

# from home this took 14 seconds 
forecast_data <- forecast_area_list %>% map_df(.f=function(FORECAST_AREA){
  forecast_url <- paste0("https://api.pjm.com/api/v1/load_frcstd_7_day?rowCount=1000&startRow=1&forecast_area=",FORECAST_AREA)
  json_forecast <- getURL(forecast_url, httpheader = c(paste0("Ocp-Apim-Subscription-Key: ", PJM_API_KEY)))
  forecast <- fromJSON(json_forecast)
  forecast_df<- forecast$items
  forecast_df
})

ESTIMATE_TIME <- ymd_hms(
  head(forecast_data$evaluated_at_datetime_ept, 1), 
  tz=tzNY)

pdf(paste0("pjm_forecast",ESTIMATE_TIME, "EPT",".pdf"), width=11, height=8.5)
#TODO: Need to get a df of the estimated peaks for the year and historical peaks for year

# all forecasts one page 
  
g_1 <-   forecast_data %>% 
    mutate(Date_Time = ymd_hms(forecast_datetime_beginning_ept, tz=tzNY)) %>% 
    ggplot(aes(x=Date_Time, y= forecast_load_mw)) + 
    geom_line() + 
    facet_wrap(~forecast_area) +
    labs(x="Date Time", y = "MW",
         title = paste0("PJM forecasted MW - All forecast areas ", 
                        "\nEvaluated at ", 
                        ESTIMATE_TIME, " EPT",  
                        "\nRef: https://api.pjm.com/api/v1/load_frcstd_7_day" )) + 
    theme_bw()
  print(g_1)  # this should be scaled to percent of peak annual forecast 

# one forecast per page
forecast_area_list %>% walk(.f=function(FORECAST_AREA){
  ESTIMATE_TIME <- ymd_hms(
      head(
        (forecast_data %>% filter(forecast_area == FORECAST_AREA))$evaluated_at_datetime_ept, 1
        ), 
    tz=tzNY)
  
 g_2 <-  forecast_data %>% 
    filter(forecast_area == FORECAST_AREA) %>% 
    mutate(Date_Time = ymd_hms(forecast_datetime_beginning_ept, tz=tzNY)) %>% 
    ggplot(aes(x=Date_Time, y= forecast_load_mw)) + 
      geom_line() + 
      labs(x="Date Time", y = "MW",
           title = paste0("PJM forecasted MW for ", 
                          FORECAST_AREA, 
                          "\nEvaluated at ", 
                          ESTIMATE_TIME, " EPT",  
           "\nRef: https://api.pjm.com/api/v1/load_frcstd_7_day" )) + 
    theme_bw()
print(g_2)

})

dev.off()







