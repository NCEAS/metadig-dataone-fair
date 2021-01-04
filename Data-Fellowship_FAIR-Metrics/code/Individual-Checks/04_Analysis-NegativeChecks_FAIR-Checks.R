#---
#  title: "FAIR Scores: Analysis of Detiorating Individual Checks"
#  author: "Christopher W. Beltz"
#  date created: "2020-11-04"
#  date updated: "2020-11-05"
#  packages updated: "2020-11-05"
#  R version: "3.6.3"
#  input: "cleaned CSV of individual FAIR scores for ADC-only data"
#  output: "..."

#---



###################################################################
####  Setup Environment
###################################################################

#load packages
library(tidyverse)
library(reshape2)
library(patchwork)
library(here)
library(dataone)
library(arcticdatautils)
library(EML)


# #THIS TAKES ~11 MINUTES TO RUN ON AURORA. FOR CONVENIENCE USE THE ARCHIVED DATA IN THE NEXT CODE CHUCK STARTING ON LINE 42.
# #Load and clean individual check data
# source(here("code", "01a_Cleaning_Individual-FAIR-Checks.R"), local = knitr::knit_global()) #This takes ~30 seconds on Aurora.
#
# #Subset cleaned data to include only original first/last versions (i.e., metadata submission and final publication), calculate change in FAIR checks over time, and calculated length of curation time (i.e., submission length).
# source(here("code", "02a_Calculation_Individual-FAIR-Checks.R"), local = knitr::knit_global()) #This takes ~10 minutes on Aurora


#load cleaned individual check data from 2020-09-30 created using the code chunk above
checks_individual_ADC <- readRDS(here("Data-Fellowship_FAIR-Metrics", "data", "Individual-Checks", "cleaned", "checks_individual_ADC_2020-09-30.rds"))
indivChecks_clean_withCalcs <- readRDS(here("Data-Fellowship_FAIR-Metrics", "data", "Individual-Checks", "cleaned", "indivChecks_clean_2020-09-30.rds"))
remove_series_id <- readRDS(here("Data-Fellowship_FAIR-Metrics", "data", "Individual-Checks", "cleaned", "remove_series_id_2020-09-30.rds"))



###################################################################
#### Data Exploration
###################################################################

#Identify which FAIR checks are deteriorating (i.e., negative 'checkDiff').
overall_mean_checkDiff <- indivChecks_clean_withCalcs %>%
  group_by(check_type, check_level, check_name) %>%
  summarise(meanDiff = mean(checkDiff),
            n_series = n()/2,
            scoreSign = sign(meanDiff))

arranged_neg_checks <- overall_mean_checkDiff  %>%
  filter(scoreSign==-1) %>%
  arrange(meanDiff)


#identify all checks with a meanDiff worse that -0.05 (i.e., 5% deterioration)
examine_top_negScores <- indivChecks_clean_withCalcs %>%
  group_by(check_type, check_level, check_name) %>%
  summarise(meanDiff = mean(checkDiff),
            scoreSign = sign(meanDiff)) %>%
  filter(scoreSign==-1) %>%
  dplyr::filter(meanDiff <= -0.05) %>%
  arrange(meanDiff)


######################################
# #get all PIDs for metadata from the ADC
######################################

#set up your nodes (in this case using the arcticdata node)
cn <- CNode("PROD")
adc <- getMNode(cn, "urn:node:ARCTIC") # the mn

#add 'origin' and 'formatId' per Jeanette's recommendation
solr_query_ADC_all <- query(adc, list(q = 'formatType:METADATA',
                                      fl = "identifier, dateUploaded, rightsHolder, title, origin, datasource, formatId",
                                      rows='50000'),
                            as = "data.frame")



######################################
# Subset solr query for negative checks
######################################

#intialize tribbles for upcoming for-loop
check_summary_root_cause <- tribble(
  ~check_name,
  ~rightsHolder,
  ~formatId,
  ~n
)

pids_negChecks <- tribble(
  ~series_id,
  ~check_level,
  ~check_id,
  ~pids_negative_unique,
  ~check_name
)

#create for loop to identify and run solr queries for records with negative mean checks
for (i in 1:length(unique(examine_top_negScores$check_name))) {

  print(examine_top_negScores$check_name[i])

  checks_with_negScores <- indivChecks_clean_withCalcs %>%
    filter(dateSplit=="INITIAL", scoreSign==-1) %>%
    filter(check_name %in% examine_top_negScores$check_name[i]) %>%
    summarise(pids_negative_unique = unique(pid),
              check_name = check_name)

  pids_negChecks <- full_join(pids_negChecks, checks_with_negScores)


  #subset solr query to only those pids that had a negative change score (i.e. deterioration)
  solr_ADC_negSubset <- solr_query_ADC_all %>%
    filter(identifier %in% checks_with_negScores$pids_negative_unique) %>%
    group_by(rightsHolder, formatId) %>%
    summarise(n=n())

  solr_ADC_negSubset$check_name <- examine_top_negScores$check_name[i]

  check_summary_root_cause <- full_join(check_summary_root_cause, solr_ADC_negSubset)

}

#write_csv(check_summary_root_cause, path = here("data", "check_this_out.csv"))


###################################################################
#### Table 1: use `gt` top look at checks with most deterioration
###################################################################
library(gt)

table1 <- check_summary_root_cause %>%
  gt::gt(groupname_col = "check_name", rowname_col = "rightsHolder") %>%
  tab_header(title = "Investigating the Source of Deteriorating FAIR Checks",
             subtitle = "the top 8 are presented and represent all checks with >5% deterioration") %>%
  cols_align(align = "center", columns = TRUE) %>%
  tab_options(
    summary_row.background.color = "dodgerblue",
    row_group.background.color = "#FFEFDB80",
    heading.background.color = "#EFFBFC",
    column_labels.background.color = "#EFFBFC",
    stub.background.color = "#EFFBFC",
    table.font.color = "#323232",
    table_body.hlines.color = "#989898",
    table_body.border.top.color = "#989898",
    heading.border.bottom.color = "#989898",
    row_group.border.top.color = "#989898",
    row_group.border.bottom.style = "none",
    stub.border.style = "dashed",
    stub.border.color = "#989898",
    stub.border.width = "1px",
    summary_row.border.color = "#989898",
    table.width = "100%")



# table1
#
# #save plot as PNG
# #NOTE: gtsave() cannot save to PNG within Aurora. Must be saved with an HTML extension if on Aurora.
# gtsave(data = table1,
#        filename="Table-01_2020-10-01_SourceOfDeterioratingChecks.png",
#        path=here("figures"))



# ###################################################################
# #### Manually investigate ISO and EML docs with negative checks
# ###################################################################
#
#
# #load data package from recent EML doc as template
# pkg_template <- get_package(adc,
#                             "resource_map_doi:10.18739/A2TB0XW4T",
#                             file_names = TRUE)
#
# doc_template <- read_eml(getObject(adc, pkg_template$metadata))
#
#
#
# ######################################
# # CHECK: "Entity Type Present"
# ######################################
#
# print("Entity Type Present")
#
# sum(pids_negChecks$check_name=="Entity Type Present")
#
# pid_ORIG <- pids_negChecks$pids_negative_unique[which(pids_negChecks$check_name=="Entity Type Present")][333]
#
# print(pid_ORIG)
#
# #load data package with most current resource map
# pkg <- get_package(adc,
#                    pid_ORIG,
#                    file_names = TRUE)
#
# all_rm_versions <- get_all_versions(adc, pkg$resource_map)
# rm_pid <- all_rm_versions[length(all_rm_versions)]
#
# pkg2 <- get_package(adc,
#                     rm_pid,
#                     file_names = TRUE)
#
# #load EML and data file
# EntityType_doc_initial <- read_eml(getObject(adc, pkg$metadata))
# EntityType_doc_final <- read_eml(getObject(adc, pkg2$metadata))
#
# #print DOI
# print(EntityType_doc_final$packageId)
#
#
#
# ######################################
# #CHECK: "Resource Revision Date Present"
# ######################################
#
# print("Resource Revision Date Present")
#
# sum(pids_negChecks$check_name=="Resource Revision Date Present")
#
# pid_ORIG <- pids_negChecks$pids_negative_unique[which(pids_negChecks$check_name=="Resource Revision Date Present")][333]
#
# print(pid_ORIG)
#
# #load data package with most current resource map
# pkg <- get_package(adc,
#                    pid_ORIG,
#                    file_names = TRUE)
#
# all_rm_versions <- get_all_versions(adc, pkg$resource_map)
# rm_pid <- all_rm_versions[length(all_rm_versions)]
#
# pkg2 <- get_package(adc,
#                     rm_pid,
#                     file_names = TRUE)
#
# #load EML and data file
# ResourceRevisionDate_doc_initial <- read_eml(getObject(adc, pkg$metadata))
# ResourceRevisionDate_doc_final <- read_eml(getObject(adc, pkg2$metadata))
#
# #print DOI
# print(ResourceRevisionDate_doc_final$packageId)
#
#
#
# ######################################
# #CHECK: "Entity Identifier Type Present"
# ######################################
#
# print("Entity Identifier Type Present")
#
# sum(pids_negChecks$check_name=="Entity Identifier Type Present")
#
# pid_ORIG <- pids_negChecks$pids_negative_unique[which(pids_negChecks$check_name=="Entity Identifier Type Present")][333]
#
# print(pid_ORIG)
#
# #load data package with most current resource map
# pkg <- get_package(adc,
#                    pid_ORIG,
#                    file_names = TRUE)
#
# all_rm_versions <- get_all_versions(adc, pkg$resource_map)
# rm_pid <- all_rm_versions[length(all_rm_versions)]
#
# pkg2 <- get_package(adc,
#                     rm_pid,
#                     file_names = TRUE)
#
# #load EML and data file
# EntityIdentifierType_doc_initial <- read_eml(getObject(adc, pkg$metadata))
# EntityIdentifierType_doc_final <- read_eml(getObject(adc, pkg2$metadata))
#
# #print DOI
# print(EntityIdentifierType_doc_final$packageId)
#
#
#
# ######################################
# #CHECK: "Entity Identifier Present (R)"
# ######################################
#
# print("Entity Identifier Present (R)")
#
# sum(pids_negChecks$check_name=="Entity Identifier Present (R)")
#
# pid_ORIG <- pids_negChecks$pids_negative_unique[which(pids_negChecks$check_name=="Entity Identifier Present (R)")][333]
#
# print(pid_ORIG)
#
# #load data package with most current resource map
# pkg <- get_package(adc,
#                    pid_ORIG,
#                    file_names = TRUE)
#
# all_rm_versions <- get_all_versions(adc, pkg$resource_map)
# rm_pid <- all_rm_versions[length(all_rm_versions)]
#
# pkg2 <- get_package(adc,
#                     rm_pid,
#                     file_names = TRUE)
#
# #load EML and data file
# EntityIdentifier.R_doc_initial <- read_eml(getObject(adc, pkg$metadata))
# EntityIdentifier.R_doc_final <- read_eml(getObject(adc, pkg2$metadata))
#
# #print DOI
# print(EntityIdentifier.R_doc_final$packageId)
#
#
#
# ######################################
# #CHECK: "Non proprietary entity format (R)"
# ######################################
#
# print("Non proprietary entity format (R)")
#
# sum(pids_negChecks$check_name=="Non proprietary entity format (R)")
#
# pid_ORIG <- pids_negChecks$pids_negative_unique[which(pids_negChecks$check_name=="Non proprietary entity format (R)")][333]
#
# print(pid_ORIG)
#
# #load data package with most current resource map
# pkg <- get_package(adc,
#                    pid_ORIG,
#                    file_names = TRUE)
#
# all_rm_versions <- get_all_versions(adc, pkg$resource_map)
# rm_pid <- all_rm_versions[length(all_rm_versions)]
#
# pkg2 <- get_package(adc,
#                     rm_pid,
#                     file_names = TRUE)
#
# #load EML and data file
# NonPropEntity.R_doc_initial <- read_eml(getObject(adc, pkg$metadata))
# NonPropEntity.R_doc_final <- read_eml(getObject(adc, pkg2$metadata))
#
# #print DOI
# print(NonPropEntity.R_doc_final$packageId)
#
#
#
# ######################################
# #CHECK: "Entity Format Present (R)"
# ######################################
#
# print("Entity Format Present (R)")
#
# sum(pids_negChecks$check_name=="Entity Format Present (R)")
#
# pid_ORIG <- pids_negChecks$pids_negative_unique[which(pids_negChecks$check_name=="Entity Format Present (R)")][333]
#
# print(pid_ORIG)
#
# #load data package with most current resource map
# pkg <- get_package(adc,
#                    pid_ORIG,
#                    file_names = TRUE)
#
# all_rm_versions <- get_all_versions(adc, pkg$resource_map)
# rm_pid <- all_rm_versions[length(all_rm_versions)]
#
# pkg2 <- get_package(adc,
#                     rm_pid,
#                     file_names = TRUE)
#
# #load EML and data file
# EntityFormat.R_doc_initial <- read_eml(getObject(adc, pkg$metadata))
# EntityFormat.R_doc_final <- read_eml(getObject(adc, pkg2$metadata))
#
# #print DOI
# print(EntityFormat.R_doc_final$packageId)
#
#
#
# ######################################
# #CHECK: "Entity Distribution URL Resolvable (R)"
# ######################################
#
# print("Entity Distribution URL Resolvable (R)")
#
# sum(pids_negChecks$check_name=="Entity Distribution URL Resolvable (R)")
#
# pid_ORIG <- pids_negChecks$pids_negative_unique[which(pids_negChecks$check_name=="Entity Distribution URL Resolvable (R)")][333]
#
# print(pid_ORIG)
#
# #load data package with most current resource map
# pkg <- get_package(adc,
#                    pid_ORIG,
#                    file_names = TRUE)
#
# all_rm_versions <- get_all_versions(adc, pkg$resource_map)
# rm_pid <- all_rm_versions[length(all_rm_versions)]
#
# pkg2 <- get_package(adc,
#                     rm_pid,
#                     file_names = TRUE)
#
# #load EML and data file
# EntityDistURL_doc_initial <- read_eml(getObject(adc, pkg$metadata))
# EntityDistURL_doc_final <- read_eml(getObject(adc, pkg2$metadata))
#
# #print DOI
# print(EntityDistURL_doc_final$packageId)


