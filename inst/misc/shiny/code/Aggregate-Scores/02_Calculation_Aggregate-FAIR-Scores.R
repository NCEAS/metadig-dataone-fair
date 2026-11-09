#---
#  title: "FAIR Scores: Subsetting and Calculating FAIR scores differences for Aggregate FAIR Scores"
#  author: "Christopher W. Beltz"
#  date created: "2020-09-21"
#  date updated: "2020-10-12"
#  packages updated: "2020-10-12"
#  R version: "3.6.3"
#  input: "cleaned CSV of aggregate FAIR scores for ADC-only data"
#  output: "cleaned CSV that contains identified first/last submissions, calculations of change in FAIR scores, and length of submission process"

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

aggChecks_firstLast <- aggChecks_clean %>%
  arrange(sequenceId, dateUploaded) %>%
  group_by(sequenceId) %>%
  slice(c(1, tail(row_number(), 1)))


#confirm numbers of remaining series_ids
# length(unique(aggChecks_clean$sequenceId)) #5894 sequenceIds retained
# nrow(aggChecks_firstLast)/2 #5894
# 6107-213 #from script 01

# #confirm two distinct dates for each sequenceId
# examine_dates_pids <- aggChecks_firstLast %>%
#   group_by(sequenceId) %>%
#   mutate(dates_unique= length(unique(dateUploaded)),
#          pids_unique= length(unique(pid)),
#          either_or_unique= dates_unique + pids_unique)
# 
# # looking for 2 unique identifiers for both dates and pids
# unique(examine_dates_pids$dates_unique) #2,1
# unique(examine_dates_pids$pids_unique) #2
# 
# # #looking for at least one of dates or filename to be unique (results 3+)
# unique(examine_dates_pids$either_or_unique) #4,3


###REMOVE UNNECESSARY STUFF
# rm(examine_dates_pids)



#############################################################
## Calculate difference in FAIR scores for each sequenceId ##
#############################################################

#identify initial/final update
aggChecks_firstLast_dateSplit <- aggChecks_firstLast %>%
  arrange(sequenceId, dateUploaded, pid) %>%
  group_by(sequenceId) %>%
  mutate(dateSplit = case_when(
    dateUploaded < lead(dateUploaded, n=1) ~ "INITIAL",
    dateUploaded > lag(dateUploaded, n=1) ~ "FINAL",
    TRUE ~ "TBD"))


# #check for TBDs
# nrow(aggChecks_firstLast_dateSplit[which(aggChecks_firstLast_dateSplit$dateSplit=="TBD"),]) #10 sequenceIds are TBD
# 
# #check for NAs
# sum(is.na(aggChecks_firstLast_dateSplit$dateSplit)) #none

#identify initial/final upload using pids for the 10 sequenceId that have identical upload datetimes.
formerTBD_dateSplit <- aggChecks_firstLast_dateSplit %>%
  filter(dateSplit=="TBD") %>%
  arrange(sequenceId, dateUploaded, pid) %>%
  group_by(sequenceId) %>%
  mutate(dateSplit = case_when(
    pid < lead(pid, n=1) ~ "INITIAL",
    pid > lag(pid, n=1) ~ "FINAL",
    TRUE ~ "NA"))

#filter TBDs out of full checks dataset 
noTBD_datesplit <- aggChecks_firstLast_dateSplit %>%
  filter(dateSplit!="TBD")


# #confirm we have the correct number of rows in separate datasets as in the full
# nrow(noTBD_datesplit) + nrow(formerTBD_dateSplit)
# nrow(aggChecks_firstLast_dateSplit)

#rejoin datasets so that INITIAL/FINAL is complete for all
aggChecks_firstLast_dateSplit_joined <- full_join(noTBD_datesplit, formerTBD_dateSplit)


# #check for TBDs
# nrow(aggChecks_firstLast_dateSplit_joined[which(aggChecks_firstLast_dateSplit_joined$dateSplit=="TBD"),]) #none
# 
# #check for NAs
# sum(is.na(aggChecks_firstLast_dateSplit_joined$dateSplit)) #none
# 
# #check for unique values in dateSplit; there should only be "INITIAL" and "FINAL"
# unique(aggChecks_firstLast_dateSplit_joined$dateSplit)


#calculate change in check status and length of curation process
aggChecks_firstLast_dateSplit_joined_calcs <- aggChecks_firstLast_dateSplit_joined %>%
  arrange(sequenceId, dateUploaded, pid) %>%
  group_by(sequenceId) %>%
  mutate(overallDIFF= scoreOverall[dateSplit=="FINAL"] - scoreOverall[dateSplit=="INITIAL"],
         findableDIFF= scoreFindable[dateSplit=="FINAL"] - scoreFindable[dateSplit=="INITIAL"],
         accessibleDIFF= scoreAccessible[dateSplit=="FINAL"] - scoreAccessible[dateSplit=="INITIAL"],
         interoperableDIFF= scoreInteroperable[dateSplit=="FINAL"] - scoreInteroperable[dateSplit=="INITIAL"],
         reusableDIFF= scoreReusable[dateSplit=="FINAL"] - scoreReusable[dateSplit=="INITIAL"])



################################################
## Extract year and month for use in plotting ##
################################################

#add year and month column
aggChecks_firstLast_dateSplit_joined_calcs <- aggChecks_firstLast_dateSplit_joined_calcs %>%
  mutate(year=lubridate::year(dateUploaded),
         month=lubridate::month(dateUploaded))



#############################################
## Change levels for easier plotting later ##
#############################################

#change levels of old/new so that "NEW" is plotted after "OLD" for overplotting
aggChecks_firstLast_dateSplit_joined_calcs$dateSplit <- factor(aggChecks_firstLast_dateSplit_joined_calcs$dateSplit, levels = c("INITIAL", "FINAL"))



#CREAT CLEANED DATASET
aggChecks_clean_withCalcs <- aggChecks_firstLast_dateSplit_joined_calcs


###REMOVE UNNECESSARY STUFF
rm(formerTBD_dateSplit, noTBD_datesplit, aggChecks_clean, aggChecks_firstLast, aggChecks_firstLast_dateSplit, aggChecks_firstLast_dateSplit_joined, aggChecks_firstLast_dateSplit_joined_calcs)


#Save to two remaining files (checks_inidividual_ADC, indivChecks_clean_withCalcs)
# saveRDS(checks_aggregate_ADC, file = here("data", "Aggregate-Scores", "cleaned", "checks_aggregate_ADC_2020-10-12.rds"))
# saveRDS(aggChecks_clean_withCalcs, file = here("data", "Aggregate-Scores", "cleaned", "aggChecks_clean_2020-10-12.rds"))


