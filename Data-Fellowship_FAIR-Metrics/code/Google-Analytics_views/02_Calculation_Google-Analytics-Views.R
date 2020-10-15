#---
#  title: "FAIR Scores: Subsetting and Calculating Google analytics view data"
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


#################################################################
## Subset data: reduce to datasets w/DOIs and viewing metrics  ##
#################################################################

GA_subset <- GA_analytics_views %>%
  filter(Category=="Dataset") %>%
  mutate(filterViews=substr(Page, 2,5)) %>%
  filter(filterViews=="view" | filterViews=="cata") %>%
  mutate(containsDOI=str_detect(Page, "doi:")) %>%
  filter(containsDOI=="TRUE")



######################################
## Trim "Page" to get only the DOI  ##
######################################

GA_subset$Page <- gsub("/catalog", "", GA_subset$Page)
GA_subset$Page <- gsub("#view", "view", GA_subset$Page)
GA_subset$Page <- gsub(" - view", "view", GA_subset$Page)
GA_subset$Page <- gsub("/quality", "", GA_subset$Page)
GA_subset$Page <- gsub("/submit", "", GA_subset$Page)
GA_subset$Page <- gsub("/view", "", GA_subset$Page)

GA_subset$Page <- gsub("resource_map_doi", "doi", GA_subset$Page)
GA_subset$Page <- gsub("resource_map_1_doi", "doi", GA_subset$Page)
GA_subset$Page <- gsub("resourceMap_doi", "doi", GA_subset$Page)
GA_subset$Page <- gsub("ddoi", "doi", GA_subset$Page)
GA_subset$Page <- gsub("doi:doi", "doi", GA_subset$Page)
GA_subset$Page <- gsub("935doi", "doi", GA_subset$Page)
GA_subset$Page <- gsub("rdoi", "doi", GA_subset$Page)
GA_subset$Page <- gsub(" doi", "doi", GA_subset$Page)
GA_subset$Page <- gsub("//doi", "/doi", GA_subset$Page)
GA_subset$Page <- gsub("/7doi", "/doi", GA_subset$Page)
GA_subset$Page <- gsub("/d1/mn/v2/object/doi", "/doi", GA_subset$Page)
GA_subset$Page <- gsub("?ref=moreLikeThis", "", GA_subset$Page)
GA_subset$Page <- gsub("<br>", "", GA_subset$Page)
GA_subset$Page <- gsub("</a>", "", GA_subset$Page)
GA_subset$Page <- gsub("</div>", "", GA_subset$Page)
GA_subset$Page <- gsub("</p>", "", GA_subset$Page)
GA_subset$Page <- gsub('[\"]', "", GA_subset$Page)
GA_subset$Page <- gsub("\\(", "", GA_subset$Page)
GA_subset$Page <- gsub("\\*", "", GA_subset$Page)
GA_subset$Page <- gsub("doi: ", "doi:", GA_subset$Page)
GA_subset$Page <- gsub("doi:352 10", "doi:10", GA_subset$Page)


#remove records with urn:uuid
sum(str_detect(GA_subset$Page, "urn:uuid"))
GA_subset <- GA_subset[-which(str_detect(GA_subset$Page, "urn:uuid")),]

#remove records with /metacat
sum(str_detect(GA_subset$Page, "/metacat"))
GA_subset <- GA_subset[-which(str_detect(GA_subset$Page, "/metacat")),]

#remove records with /doi:/User
sum(str_detect(GA_subset$Page, "/doi:/User"))
GA_subset <- GA_subset[-which(str_detect(GA_subset$Page, "/doi:/User")),]

#remove records with /https:
sum(str_detect(GA_subset$Page, "/https:"))
GA_subset <- GA_subset[-which(str_detect(GA_subset$Page, "/https:")),]

#remove records with /doi:ess-d"
sum(str_detect(GA_subset$Page, "/doi:ess-d"))
GA_subset <- GA_subset[-which(str_detect(GA_subset$Page, "/doi:ess-d")),]


#clean "/doi" to "doi"
GA_subset$Page <- gsub("/doi:", "doi:", GA_subset$Page)

#remove whitespace at beginning of doi
GA_subset$Page <- str_trim(GA_subset$Page, side="both")


#remove records with incomplete DOIs
GA_subset <- GA_subset[-which(str_sub(GA_subset$Page, start=1, end=10)=="doi:"),]
GA_subset <- GA_subset[-which(str_sub(GA_subset$Page, start=1, end=10)=="doi:1"),]
GA_subset <- GA_subset[-which(str_sub(GA_subset$Page, start=1, end=10)=="doi:10"),]


#remove other records with things at beginning of doi string
namesX <- str_sub(GA_subset$Page, start=10, end=10)
unique(namesX)



##############################################################
## Combine disparate DOI views to most current version DOI  ##
##############################################################






##############################
## Remove unneeded columns  ##
##############################

GA_subset <- GA_subset %>%
  select(-Category, -filterViews, -containsDOI)



##########################
## Write out clean CSV  ##
##########################

GA_views_clean <- GA_subset

#Save to three remaining files (checks_inidividual_ADC, indivChecks_clean_withCalcs, remove_series_id)
# saveRDS(GA_views_clean, file = here("data", "Google-Analytics_views", "cleaned", "GA_views_clean_2020-10-15.rds"))


##REMOVE UNNECESSARY STUFF
rm(GA_subset, namesX)

