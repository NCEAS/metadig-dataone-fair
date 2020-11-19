#---
#  title: "FAIR Scores: Cleaning Google Analytics data"
#  author: "Christopher W. Beltz"
#  date created: "2020-10-13"
#  date updated: "2020-10-13"
#  packages updated: "2020-10-13"
#  R version: "3.6.3"
#  input: "..."
#  output: "..."

#---


#load packages
library(dataone)
library(EML)
library(emld)
library(tidyverse)
library(reshape2)
library(patchwork)
library(here)


###########################################################################################
#####                   GET GOOGLE ANALYTICS VIEW DATA                                #####
###########################################################################################

#get path to aggregate FAIR scores for ADC data ONLY using locally stored copy (updated 2020-09-08)
path_2016_google_analytics_views <- here("data", "Google-Analytics_views", "raw", "2016-GA-Analytics Data - Sheet1.csv")
path_2017_google_analytics_views <- here("data", "Google-Analytics_views", "raw", "2017-GA-Analytics Data - Sheet1.csv")
path_2018_google_analytics_views <- here("data", "Google-Analytics_views", "raw", "2018-GA-Analytics Data - Sheet1.csv")
path_2019_google_analytics_views <- here("data", "Google-Analytics_views", "raw", "2019-GA-Analytics Data - Sheet1.csv")
path_2020_google_analytics_views <- here("data", "Google-Analytics_views", "raw", "2020-GA-Analytics Data - Sheet1.csv")

#load aggregate FAIR scores
google_analytics_views_2016 <- read_csv(file=path_2016_google_analytics_views)
google_analytics_views_2017 <- read_csv(file=path_2017_google_analytics_views)
google_analytics_views_2018 <- read_csv(file=path_2018_google_analytics_views)
google_analytics_views_2019 <- read_csv(file=path_2019_google_analytics_views)
google_analytics_views_2020 <- read_csv(file=path_2020_google_analytics_views)

###REMOVE UNNECESSARY STUFF
rm(path_2016_google_analytics_views, path_2017_google_analytics_views, path_2018_google_analytics_views, path_2019_google_analytics_views, path_2020_google_analytics_views)



##########################################
## Aggregate data into single dataframe ##
##########################################

GA_complete <- full_join(google_analytics_views_2016, google_analytics_views_2017) %>%
  full_join(google_analytics_views_2018) %>%
  full_join(google_analytics_views_2019) %>%
  full_join(google_analytics_views_2020)

###REMOVE UNNECESSARY STUFF
rm(google_analytics_views_2016, google_analytics_views_2017, google_analytics_views_2018, google_analytics_views_2019, google_analytics_views_2020)



##############################
## Create cleaned dataframe ##
##############################

#CREATE CLEANED DATASET
GA_analytics_views <- GA_complete


###REMOVE UNNECESSARY STUFF
rm(GA_complete)


