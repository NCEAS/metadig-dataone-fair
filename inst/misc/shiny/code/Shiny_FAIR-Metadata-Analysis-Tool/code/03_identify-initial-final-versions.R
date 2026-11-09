#---
#  title: "FAIR Scores: Creating a Shiny Dashboard"
#  author: "Christopher W. Beltz"
#  date created: "2020-12-02"
#  R version: "4.0.2"
#  input: "NA"
#  output: "NA"

#---



#############################################################
## Calculate difference in FAIR scores for each sequenceId ##
#############################################################

#identify initial/final update
aggScore_dateSplit <- aggScore_clean %>%
  group_by(sequenceId) %>%
  arrange(dateUploaded, pid) %>%
  mutate(dateSplit = case_when(
    dateUploaded == min(dateUploaded) ~ "INITIAL",
    dateUploaded == max(dateUploaded) ~ "FINAL",
    TRUE ~ "INTERMEDIATE"))


aggScore_clean <- aggScore_dateSplit
