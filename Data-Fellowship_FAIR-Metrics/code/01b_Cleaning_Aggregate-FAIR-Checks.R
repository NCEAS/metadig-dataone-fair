#---
#  title: "FAIR Scores: Cleaning Aggregate Check Data"
#  author: "Christopher W. Beltz"
#  date created: "2020-09-21"
#  date updated: "2020-10-06"
#  packages updated: "2020-10-06"
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
path_arctic_aggregate_checks <- here("data", "scores_checks", "raw", "output_2020-10-06_check-aggregate-data.csv")

#load aggregate FAIR scores
checks_aggregate_ADC <- read_csv(file=path_arctic_aggregate_checks)


###REMOVE UNNECESSARY STUFF
rm(path_arctic_aggregate_checks)