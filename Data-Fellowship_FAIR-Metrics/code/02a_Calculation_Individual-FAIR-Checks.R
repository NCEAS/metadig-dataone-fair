#---
#  title: "FAIR Scores: Subsetting and Calculating FAIR check differences for Individual Check Data"
#  author: "Christopher W. Beltz"
#  date created: "2020-09-21"
#  date updated: "2020-09-30"
#  packages updated: "2020-09-30"
#  R version: "3.6.3"
#  input: "cleaned CSV of individual checks for ADC-only data"
#  output: "cleaned CSV that contains identified first/last submissions, calculations of change in FAIR checks, and length of submission process"

#---


#load packages
library(dataone)
library(EML)
library(emld)
library(tidyverse)
library(reshape2)
library(patchwork)
library(here)


###################################################################
## Subset the data by removing all but the first and last upload ##
###################################################################

#keep only the most recent and the oldest version of each of the 51 checks via series_id
indivChecks_firstLast <- indivChecks_clean %>%
  arrange(series_id, date_uploaded, filename) %>%
  group_by(series_id) %>%
  slice(c(1:51, tail(row_number(), 51)))


#confirm numbers of remaining series_ids
# length(unique(indivChecks_clean$series_id)) #5893 series_ids retained
# nrow(indivChecks_firstLast)/51/2
# 6105-212 #from script 01a

#confirm two distinct dates and/or filenames for each series_id
# examine_dates_filenames <- indivChecks_firstLast %>%
#   group_by(series_id) %>%
#   mutate(dates_unique= length(unique(date_uploaded)),
#          filenames_unique=length(unique(filename)),
#          either_or_unique= dates_unique + filenames_unique)
# 
# #looking for 2 unique identifiers for both dates and filenames
# unique(examine_dates_filenames$dates_unique) #2,1
# unique(examine_dates_filenames$filenames_unique) #2
# 
# #looking for at least one of dates or filename to be unique (results 3+)
# unique(examine_dates_filenames$either_or_unique) #4,3

###REMOVE UNNECESSARY STUFF
#rm(examine_dates_filenames)


########################################
## Check for "ERROR" in check status  ##
########################################

#look for any errors in check_status
unique(indivChecks_firstLast$check_status)

#how many errors?
sum(indivChecks_firstLast$check_status=="ERROR") #3

#across how many series_ids?
series_with_errors <- unique(indivChecks_firstLast$series_id[indivChecks_firstLast$check_status=="ERROR"]) #3

#remove series_ids with errors
indivChecks_firstLast <- indivChecks_firstLast %>%
  filter(!series_id %in% series_with_errors)



##################################################################
## Calculate difference in individual checks for each series_id ##
##################################################################

#assign 1 to a successful check and 0 to a failure; also identify initial/final update
indivChecks_firstLast_dateSplit <- indivChecks_firstLast %>%
  mutate(check_status_num = case_when(
    check_status == "SUCCESS" ~ 1,
    check_status == "FAILURE" ~ 0)) %>%
  arrange(series_id, check_level, check_id, date_uploaded, filename) %>%
  group_by(series_id, check_level, check_id) %>%
  mutate(dateSplit = case_when(
    date_uploaded < lead(date_uploaded, n=1) ~ "INITIAL",
    date_uploaded > lag(date_uploaded, n=1) ~ "FINAL",
    TRUE ~ "TBD"))


# #check for TBDs
# nrow(indivChecks_firstLast_dateSplit[which(indivChecks_firstLast_dateSplit$dateSplit=="TBD"),])/51 #10 series_ids are TBD
# 
# #check for NAs
# sum(is.na(indivChecks_firstLast_dateSplit$dateSplit))/51 #none

#identify initial/final upload using filenames for the 10 series_ids that have identical upload datetimes.
formerTBD_dateSplit <- indivChecks_firstLast_dateSplit %>%
  filter(dateSplit=="TBD") %>%
  arrange(series_id, check_level, check_id, date_uploaded, filename) %>%
  group_by(series_id, check_level, check_id) %>%
  mutate(dateSplit = case_when(
    filename < lead(filename, n=1) ~ "INITIAL",
    filename > lag(filename, n=1) ~ "FINAL",
    TRUE ~ "NA"))

#filter TBDs out of full checks dataset 
noTBD_datesplit <- indivChecks_firstLast_dateSplit %>%
  filter(dateSplit!="TBD")

# #confirm we have the correct number of rows in separate datasets as in the full
# nrow(noTBD_datesplit) + nrow(formerTBD_dateSplit)
# nrow(indivChecks_firstLast_dateSplit)

#rejoin datasets so that INITIAL/FINAL is complete for all
indivChecks_firstLast_dateSplit_joined <- full_join(noTBD_datesplit, formerTBD_dateSplit)

# #check for TBDs
# nrow(indivChecks_firstLast_dateSplit_joined[which(indivChecks_firstLast_dateSplit_joined$dateSplit=="TBD"),])/51 #none
# 
# #check for NAs
# sum(is.na(indivChecks_firstLast_dateSplit_joined$dateSplit))/51 #none
# 
# #check for unique values in dateSplit; there should only be "INITIAL" and "FINAL"
# unique(indivChecks_firstLast_dateSplit_joined$dateSplit)


#calculate change in check status and length of curation process
indivChecks_firstLast_dateSplit_joined_calcs <- indivChecks_firstLast_dateSplit_joined %>%
  arrange(series_id, check_level, check_id, date_uploaded, filename) %>%
  group_by(series_id, check_level, check_id) %>%
  mutate(checkDiff = check_status_num[dateSplit=="FINAL"] - check_status_num[dateSplit=="INITIAL"],
         scoreSign = sign(checkDiff))



###############################################################
## Calculate length of submission process for each series_id ##
###############################################################

#calculate difference between time of submission and final publication for each series_id
#NOTE: This calculation takes ~3 minutes using 'difftime()' wrapped in 'as.numeric()' on Aurora. Otherwise it takes ~30min.
indivChecks_firstLast_dateSplit_joined_calcs_submissionLength <- indivChecks_firstLast_dateSplit_joined_calcs %>%
  arrange(series_id, check_level, check_id, date_uploaded, filename) %>%
  group_by(series_id, check_level, check_id) %>%
  mutate(submissionLength_days= as.numeric(difftime(date_uploaded[dateSplit=="FINAL"], date_uploaded[dateSplit=="INITIAL"], units="days")))



#############################################
## Change levels for easier plotting later ##
#############################################

#change levels of old/new so that "NEW" is plotted after "OLD" for overplotting
indivChecks_firstLast_dateSplit_joined_calcs_submissionLength$dateSplit <- factor(indivChecks_firstLast_dateSplit_joined_calcs_submissionLength$dateSplit, levels = c("INITIAL", "FINAL"))

#change levels of REQUIRED/OPTIONAL so that "REQUIRED" checks are plotted at top of each facet above "OPTIONAL"
indivChecks_firstLast_dateSplit_joined_calcs_submissionLength$check_level <- factor(indivChecks_firstLast_dateSplit_joined_calcs_submissionLength$check_level, levels = c("REQUIRED", "OPTIONAL"))



#CREAT CLEANED DATASET
indivChecks_clean_withCalcs <- indivChecks_firstLast_dateSplit_joined_calcs_submissionLength


###REMOVE UNNECESSARY STUFF
rm(formerTBD_dateSplit, noTBD_datesplit, indivChecks_clean, indivChecks_firstLast, indivChecks_firstLast_dateSplit, indivChecks_firstLast_dateSplit_joined, indivChecks_firstLast_dateSplit_joined_calcs, indivChecks_firstLast_dateSplit_joined_calcs_submissionLength)


#Save to three remaining files (checks_inidividual_ADC, indivChecks_clean_withCalcs, remove_series_id)
# saveRDS(checks_individual_ADC, file = here("FAIR-Checks", "data", "cleaned", "checks_individual_ADC_2020-09-30.rds"))
# saveRDS(indivChecks_clean_withCalcs, file = here("FAIR-Checks", "data", "cleaned", "indivChecks_clean_2020-09-30.rds"))
# saveRDS(remove_series_id, file = here("FAIR-Checks", "data", "cleaned", "remove_series_id_2020-09-30.rds"))


