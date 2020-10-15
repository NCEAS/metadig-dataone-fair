#---
#  title: "FAIR Scores: Cleaning Aggregate Check Data"
#  author: "Christopher W. Beltz"
#  date created: "2020-09-21"
#  date updated: "2020-10-12"
#  packages updated: "2020-10-12"
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


#####################################################################################
#####                   GET AGGREGATE CHECK DATA                                #####
#####################################################################################

#Get updated aggregate check scores via terminal
# curl -v --GET -H "Accept: text/csv" "https://docker-ucsb-4.dataone.org:30443/quality/scores/?id=urn:node:ARCTIC&suite=FAIR-suite-0.3.1"
# add -o "[filname].csv"

#get path to aggregate FAIR scores for ADC data ONLY (from Chris Beltz's drive on Aurora)
#path_arctic_aggregate_checks <- "/home/cwbeltz/FAIR-Checks/data/output_2020-09-08_check-aggregate-data.csv"

#get path to aggregate FAIR scores for ADC data ONLY using locally stored copy (updated 2020-09-08)
path_arctic_aggregate_checks <- here("data", "Aggregate-Scores", "raw", "output_2020-10-06_check-aggregate-data.csv")

#load aggregate FAIR scores
checks_aggregate_ADC <- read_csv(file=path_arctic_aggregate_checks)


###REMOVE UNNECESSARY STUFF
rm(path_arctic_aggregate_checks)



####################################################################################################
## Confirm data only comes from Arctic Data Center is include and structure/names are appropriate ##
####################################################################################################

#only ADC data
#unique(checks_aggregate_ADC$datasource)

#check structure and column names
#str(checks_aggregate_ADC)



#########################################################################
## Clean data: check for sequenceId and unique pids w/in a sequenceId  ##
#########################################################################

#NOTE: A sequenceId is the equivalent of a reverse engineered series_id and uniquely identifies a 
#      individual entity across multiple versions/pids.


#count datasets in ADC data using sequenceIds
#length(unique(checks_aggregate_ADC$sequenceId)) #TOTAL=6108 (individual checks were 6105 at last run)

#check for NA in sequenceId
#sum(is.na(checks_aggregate_ADC$sequenceId)) #217 (same as individual checks at last run)

#remove checks and docs that are missing a 'sequenceId'
checks_agg_seqPos_ADC <- checks_aggregate_ADC[-which(is.na(checks_aggregate_ADC$sequenceId)),]

#CHECKPOINT: datasets remaining using sequenceIds
#length(unique(checks_agg_seqPos_ADC$sequenceId)) #TOTAL=6107 (only 1 sequenceId, but 217 records/rows/pids; same as individual checks)


#examine series_ids that do not have 2 unique PIDs
examine_unique_pids <- checks_agg_seqPos_ADC %>%
  arrange(sequenceId, dateUploaded) %>%
  group_by(sequenceId) %>%
  summarise(unique_pids= length(unique(pid)))

#number of datasets removed for having only one unique pid within the sequenceId
#sum(examine_unique_pids$unique_pids[examine_unique_pids$unique_pids==1]) #213 removed

#create vector for sequenceIds with only 1 unique pid
remove_sequenceId <- examine_unique_pids$sequenceId[examine_unique_pids$unique_pids==1]

#remove series_id with only 1 unique pid
checks_agg_seqPos_pidUnique_ADC <- checks_agg_seqPos_ADC[-which(checks_agg_seqPos_ADC$sequenceId %in% remove_sequenceId),]


#CHECKPOINT:datasets remaining using series_ids
#length(unique(checks_agg_seqPos_pidUnique_ADC$sequenceId)) #TOTAL=5894 (1 more than individual checks)


#CREAT CLEANED DATASET
aggChecks_clean <- checks_agg_seqPos_pidUnique_ADC


###REMOVE UNNECESSARY STUFF
rm(examine_unique_pids, remove_sequenceId, checks_agg_seqPos_ADC, checks_agg_seqPos_pidUnique_ADC)


