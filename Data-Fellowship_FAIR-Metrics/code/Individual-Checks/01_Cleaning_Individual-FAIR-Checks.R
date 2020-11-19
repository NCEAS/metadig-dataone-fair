#---
#  title: "FAIR Scores: Cleaning Individual Check Data"
#  author: "Christopher W. Beltz"
#  date created: "2020-09-21"
#  date updated: "2020-10-12"
#  packages updated: "2020-10-12"
#  R version: "3.6.3"
#  input: "uncleaned RDA with large CSV of individual checks for ADC and LTER"
#  output: "cleaned CSV to RDS of individual checks for ADC-only data"

#---


#load packages
library(dataone)
library(EML)
library(emld)
library(tidyverse)
library(reshape2)
library(patchwork)
library(here)


#####################################################################################
#####                  GET INDIVIDUAL CHECK DATA                                #####
#####################################################################################

#get path to FAIR checks for ADC data ONLY (direct from Peter Slaughter's drive on Aurora)
#path_arctic_individual_checks <- "/home/slaughter/FAIR-suite-0.3.1/ARCTIC/check-data/fair-0.3.1-checks-joined.rda"

#get path to FAIR checks for ADC data ONLY using locally stored copy at /home/cwbeltz/FAIR-Checks
path_arctic_individual_checks <- here("data", "Individual-Checks", "raw", "fair-0.3.1-checks-joined.rda")

#load ADC checks
load(file = path_arctic_individual_checks)


###REMOVE UNNECESSARY STUFF
rm(path_arctic_individual_checks)



#############################################################################################
## Clean data: subset data so only Arctic Data Center is include and fix spelling mistakes ##
#############################################################################################

#subset for ADC-only data (i.e., remove LTER)
checks_individual_ADC <- checks_joined %>%
  filter(origin_mn %in% c("urn:node:ARCTIC"))

#fix spelling in column names
colnames(checks_individual_ADC)[which(names(checks_individual_ADC) == "date_uplaoded")] <- "date_uploaded"

#remove the period (i.e., ".") from check name
checks_individual_ADC$check_name <- gsub("Entity checksum and algorithm are present.", "Entity checksum and algorithm are present", checks_individual_ADC$check_name)

#add "(R)" to required checks levels
checks_individual_ADC <- checks_individual_ADC %>%
  mutate(check_name = case_when(
    check_level == "REQUIRED" ~ paste0(check_name, " (R)"),
    check_level == "OPTIONAL" ~ paste0(check_name)))

#add year and month column
checks_individual_ADC <- checks_individual_ADC %>%
  mutate(year=lubridate::year(date_uploaded),
         month=lubridate::month(date_uploaded))



#######################################################################
## Clean data: check for series_id and unique pids w/in a series_id  ##
#######################################################################

#count datasets in ADC data using series_ids
#length(unique(checks_individual_ADC$series_id)) #TOTAL=6105


#remove checks and docs that are missing a 'series_id'
checks_indiv_seriesPos_ADC <- checks_individual_ADC[-which(is.na(checks_individual_ADC$series_id)),]

#number of datasets removed for missing 'series_id'
#sum(is.na(checks_individual_ADC$series_id))/51 #1 series_id/217 docs removed

#CHECKPOINT: datasets remaining using series_ids
#length(unique(checks_indiv_seriesPos_ADC$series_id)) #TOTAL=6104


#examine series_ids that do not have 2 unique PIDs
examine_unique_pids <- checks_indiv_seriesPos_ADC %>%
  arrange(series_id, date_uploaded) %>%
  group_by(series_id) %>%
  summarise(unique_pids= length(unique(pid)))

#number of datasets removed for having only one unique pid within the 'series_id'
#sum(examine_unique_pids$unique_pids[examine_unique_pids$unique_pids==1]) #211 removed

#create vector for series_ids with only 1 unique pid
remove_series_id <- examine_unique_pids$series_id[examine_unique_pids$unique_pids==1]

#remove series_id with only 1 unique pid
checks_indiv_seriesPos_pidUnique_ADC <- checks_indiv_seriesPos_ADC[-which(checks_indiv_seriesPos_ADC$series_id %in% remove_series_id),]

#CHECKPOINT:datasets remaining using series_ids
#length(unique(checks_indiv_seriesPos_pidUnique_ADC$series_id)) #TOTAL=5893


#CREAT CLEANED DATASET
indivChecks_clean <- checks_indiv_seriesPos_pidUnique_ADC


###REMOVE UNNECESSARY STUFF
rm(examine_unique_pids, checks_indiv_seriesPos_ADC, checks_indiv_seriesPos_pidUnique_ADC, checks_joined)


