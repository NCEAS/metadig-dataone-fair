#---
#  title: "FAIR Scores: Creating a Shiny Dashboard"
#  author: "Christopher W. Beltz"
#  date created: "2020-11-30"
#  R version: "4.0.2"
#  input: "NA"
#  output: "raw data for aggregate FAIR scores from the ADC"

#---



#####################################################################################
#####                   GET UPDATED AGGREGATE CHECK DATA                        #####
#####################################################################################

#Get updated aggregate FAIR scores data via terminal (data should be current to within minutes)
# curl -v --GET -H "Accept: text/csv" "https://docker-ucsb-4.dataone.org:30443/quality/scores/?id=urn:node:ARCTIC&suite=FAIR-suite-0.3.1"
# -o "[filname].csv"
# NOTE: run the above two lines together



#####################################################################################
#####                        LOAD AGGREGATE CHECK DATA                          #####
#####################################################################################

#get path to aggregate FAIR scores for ADC data using locally stored copy
path_ADC_aggregate_score <- here("data", "raw", "output_2021-01-04_aggregate-score-data.csv")

#load aggregate FAIR scores
aggregate_score_ADC <- read_csv(file=path_ADC_aggregate_score)





###REMOVE UNNECESSARY STUFF
rm(path_ADC_aggregate_score)


