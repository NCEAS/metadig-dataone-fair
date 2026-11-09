#---
#  title: "FAIR Scores: Creating a Shiny Dashboard"
#  author: "Christopher W. Beltz"
#  date created: "2020-12-03"
#  R version: "4.0.2"
#  input: "NA"
#  output: "NA"

#---



############################################
## Identify datasets published with a DOI ##
############################################

aggScore_clean <- aggScore_clean %>% 
  mutate(DOI_present = case_when(
    "doi" == substr(pid,1,3) ~ "DOI",
    TRUE ~ "none"))

aggScore_clean$DOI_present <- factor(aggScore_clean$DOI_present)

  


