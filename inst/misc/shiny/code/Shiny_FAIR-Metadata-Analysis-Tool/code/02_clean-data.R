#---
#  title: "FAIR Scores: Creating a Shiny Dashboard"
#  author: "Christopher W. Beltz"
#  date created: "2020-11-30"
#  R version: "4.0.2"
#  input: "raw data for aggregate FAIR scores from the ADC"
#  output: "cleaned data for aggregate FAIR scores"

#---



####################################################################################################
## Confirm data only comes from Arctic Data Center is include and structure/names are appropriate ##
####################################################################################################

#only ADC data
# unique(aggregate_score_ADC$datasource)

#check structure and column names
# str(aggregate_score_ADC)



#########################################################################
## Clean data: check for sequenceId and unique pids w/in a sequenceId  ##
#########################################################################

#NOTE: A sequenceId is the equivalent of a reverse engineered series_id and uniquely identifies a 
#      individual entity across multiple versions/pids.


# #CHECKPOINT: count datasets in ADC data using sequenceIds
# length(unique(aggregate_score_ADC$sequenceId)) #TOTAL=6156

# #CHECKPOINT: check for NA in sequenceId
# sum(is.na(aggregate_score_ADC$sequenceId)) #407

#remove checks and docs that are missing a 'sequenceId'
agg_score_seqPos_ADC <- aggregate_score_ADC[-which(is.na(aggregate_score_ADC$sequenceId)),]

# #CHECKPOINT: datasets remaining using sequenceIds
# length(unique(agg_score_seqPos_ADC$sequenceId)) #TOTAL=6155 


#examine series_ids that do not have 2 unique PIDs
examine_unique_pids <- agg_score_seqPos_ADC %>%
  arrange(sequenceId, dateUploaded) %>%
  group_by(sequenceId) %>%
  summarise(unique_pids= length(unique(pid)))

# #number of datasets removed for having only one unique pid within the sequenceId
# sum(examine_unique_pids$unique_pids[examine_unique_pids$unique_pids==1]) #223 removed

#create vector for sequenceIds with only 1 unique pid
remove_sequenceId <- examine_unique_pids$sequenceId[examine_unique_pids$unique_pids==1]

#remove series_id with only 1 unique pid
agg_score_seqPos_pidUnique_ADC <- agg_score_seqPos_ADC[-which(agg_score_seqPos_ADC$sequenceId %in% remove_sequenceId),]


# #CHECKPOINT:datasets remaining using series_ids
# length(unique(agg_score_seqPos_pidUnique_ADC$sequenceId)) #TOTAL=5932


#CREAT CLEANED DATASET
aggScore_clean <- agg_score_seqPos_pidUnique_ADC


###REMOVE UNNECESSARY STUFF
rm(examine_unique_pids, remove_sequenceId, agg_score_seqPos_ADC, agg_score_seqPos_pidUnique_ADC)



