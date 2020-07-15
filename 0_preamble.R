# =========================================#
#                                          #
#     DIGITAL FIRST PRIMARY CARE STUDY     #
#                                          #
#        AskmyGP Phase I Analysis          #
# -----------------------------------------#
#                                          #
# (0_preamble.r):                          #
#                                          #
#  (1) Loads libraries                     #
#  (2) Defines useful variables            #
# -----------------------------------------#
#                                          #
# Authors:                                 #
# Geraldine Clarke                         #
# Paris Pariza                             #
#                                          #
# =========================================#


# Load libraries ----

library(data.table)
library(tidyverse)
library(lubridate)
library(glue)
library(here)
library(forcats)
library(janitor)
library(stringr)
library(flextable)
library("RColorBrewer")


##Define useful variables ----

version="V5"

#For data

early_adopters_end <-'2019_02'
late_adopters_end <- '2020_02'

early_adopters_dates <- c("2018_07", "2018_08", "2018_09", "2018_10", "2018_11", "2018_12", "2019_01", "2019_02", "2019_03", "2019_04", "2019_05", "2019_06", "2019_07", "2019_08", "2019_09",
                          "2019_10", "2019_11", "2019_12", "2020_01", "2020_02", "2020_03" , "2020_04", "2020_05", "2020_06", "2020_07")
         
late_adopters_dates <- c("2019_02", "2019_03", "2019_04", "2019_05", "2019_06", "2019_07", "2019_08", "2019_09",
                         "2019_10", "2019_11", "2019_12", "2020_01", "2020_02", "2020_03" , "2020_04", "2020_05", "2020_06", "2020_07")

pre_covid_adopters_dates <- c("2018_07", "2018_08", "2018_09", "2018_10", "2018_11", "2018_12", "2019_01", "2019_02", "2019_03", "2019_04", "2019_05", "2019_06", "2019_07", "2019_08", "2019_09",
                         "2019_10", "2019_11", "2019_12", "2020_01", "2020_02" )    

covid_adopters_dates <- c("2020_03" , "2020_04", "2020_05", "2020_06", "2020_07") 

dates_2019 <- c("2019_01", "2019_02", "2019_03", "2019_04", "2019_05", "2019_06", "2019_07", "2019_08", "2019_09",
                         "2019_10", "2019_11", "2019_12")

age_group_0 <- c("0-4", "5-9", "10-14", "15-19")
age_group_20 <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64") 
age_group_65 <- c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+")

months = c(1:12)
months = ifelse(months<10, paste0("0", months), months)
study_year_month = c(
  paste("2016", months, sep="_" ),
  paste("2017", months, sep="_" ),
  paste("2018", months, sep="_" ),
  paste("2019", months, sep="_" ),
  paste("2020", months[1:6], sep="_"))


# For plots

study_year_month_plot = c(study_year_month[-c(1:36)]) 
study_year_month_plot_fft = c(study_year_month[-c(1:40)]) 

closure_types_all =  c("Abandoned", "Autocompleted", "Face to face", "Online message", 
                       "Phone call", "Unknown", "Video", "Visit")  
closure_types_active =  c("Face to face", "Online message", "Phone call", "Video", "Visit")  
closure_types_active_triage =  c("Autocompleted", "Face to face", "Online message", "Phone call", "Video", "Visit")  
closure_types_active_volume =  c("Face to face", "Online message", "Phone call")  
closure_types_remote = c("Online message","Phone call","Video")
closure_types_non_remote = c("Face to face", "Visit")
closure_types_ok = c("Face to face", "Online message", "Phone call", "Unknown", "Video", "Visit")
closure_types_ok_known = c("Face to face", "Online message", "Phone call", "Video", "Visit")
requested_types_active <- c("Face to face", "Online message", "Phone call", "Video")

covid = c("covid_adopter")
non_covid = c("early_adopter", "late_adopter")
list_starts = list(non_covid=non_covid, covid=covid)
