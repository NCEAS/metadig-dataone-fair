#---
#  title: "FAIR Scores: Creating a Shiny Dashboard"
#  author: "Christopher W. Beltz"
#  date created: "2020-12-01"
#  R version: "4.0.2"
#  input: "NA"
#  output: "NA"

#---



#find date of most recent upload
most_recent_upload <- data.frame(matrix(ncol = 0, nrow = 1))
most_recent_upload$date <- max(aggScore_clean$dateUploaded)
most_recent_upload$pid <- aggScore_clean$pid[which(aggScore_clean$dateUploaded==most_recent_upload$date)]



#calculate monthly mean scores
monthly_mean_scores <- aggScore_clean %>% 
  filter(dateUploaded > (Sys.time()-lubridate::days(365*1))) %>%
  mutate(month_floor = lubridate::floor_date(dateUploaded, unit="month")) %>% 
  pivot_longer(cols= c("scoreOverall", "scoreFindable", "scoreAccessible", "scoreInteroperable", "scoreReusable"), names_to = "type", values_to = "score")
  
monthly_mean_scores <- monthly_mean_scores %>%
  group_by(month_floor, type) %>%
  summarise(mean = mean(score)) %>% 
  group_by(type) %>% 
  summarise(mean_monthly_score = mean(mean),
            sd_monthly_score = sd(mean))
 





