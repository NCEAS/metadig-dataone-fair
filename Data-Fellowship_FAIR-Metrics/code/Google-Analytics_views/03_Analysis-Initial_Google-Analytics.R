#---
#  title: "FAIR Scores: Initial Analysis of Google Analytics view data"
#  author: "Christopher W. Beltz"
#  date created: "2020-11-05"
#  date updated: "2020-11-06"
#  packages updated: "2020-11-06"
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



###################################################################
#### Load Common Graphics
###################################################################

#load common graphical parameters, based on `theme_ADC` from "github.nceas.ucsb.edu/KNB/arctic-data/blob/master/reporting/R/theme_ADC.R"
source(here("code", "Graphical", "theme_ADC_modified.R"))



###################################################################
#### Load Data
###################################################################

#load cleaned Google analytics data from 2020-10-15 using the code above
GA_views_clean <- readRDS(here("data", "Google-Analytics_views", "cleaned", "GA_views_clean_2020-10-15.rds"))

#load cleaned aggregate score data from 2020-10-12 created using the code chunk above
checks_aggregate_ADC <- readRDS(here("data", "Aggregate-Scores", "cleaned", "checks_aggregate_ADC_2020-10-12.rds"))
aggChecks_clean_withCalcs <- readRDS(here("data", "Aggregate-Scores", "cleaned", "aggChecks_clean_2020-10-12.rds"))



###################################################################
#### Data Wrangling
###################################################################

#summarize views for each DOI
GA_summary <- GA_views_clean %>%
  group_by(Page) %>%
  summarise(totalViews=sum(Pageviews),
            totalUniqueViews=sum(`Unique Pageviews`))

#subset FAIR scores for final metadata versions and clean up DOIs to match GA views
aggChecks_summary <- aggChecks_clean_withCalcs %>%
  filter(dateSplit=="FINAL") %>%
  mutate(containsDOI=str_detect(pid, "doi:")) %>%
  filter(containsDOI=="TRUE")

#merge data by DOI
agg_GA_joined <- left_join(aggChecks_summary, GA_summary, by=c("pid"="Page"))

#remove NAs
agg_GA_joined <- agg_GA_joined[!is.na(agg_GA_joined$totalViews),]



######################################
# Variables to be called
######################################

#number of datasets after merging and removing NAs
agg_GA_datasets <- nrow(agg_GA_joined)



######################################
# Figure 8a:
######################################

plot8a <- ggplot(agg_GA_joined, aes(x=scoreOverall, y=totalUniqueViews)) + 
  geom_point() +
  geom_smooth(method="gam") +
  xlab("Overall FAIR Score") +
  ylab("Total Unique Views") +
  theme_ADC_modified


#create final plot
plot8a <- plot8a + plot_annotation(
  title = 'Relationship between FAIR and web views',
  subtitle = 'for all datasets since inception of ADC',
  caption = 'updated 2020-11-06
  data current on 2020-10-12 (FAIR) and 2020-10-15 (GA)')


# plot8a
# 
# #save plot as PNG to Aurora
# ggsave(filename="Figure-08a_2020-11-06_Google-Analytics-with-FAIR.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)



######################################
# Figure 8b:
######################################

plot8b <- ggplot(agg_GA_joined[agg_GA_joined$scoreOverall>0.55,], aes(x=scoreOverall, y=totalUniqueViews)) + 
  geom_point() +
  geom_smooth(method="gam") +
  xlab("Overall FAIR Score") +
  ylab("Total Unique Views") +
  theme_ADC_modified


#create final plot
plot8b <- plot8b + plot_annotation(
  title = 'Relationship between FAIR and web views',
  subtitle = 'for all datasets with Overal FAIR score >0.50',
  caption = 'updated 2020-11-06
  data current on 2020-10-12 (FAIR) and 2020-10-15 (GA)')


# plot8b
# 
# 
# #save plot as PNG to Aurora
# ggsave(filename="Figure-08b_2020-11-06_Google-Analytics-with-FAIR.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)



######################################
# Figure 8c:
######################################

#remove everything with less than 5 views
agg_GA_joined_overFive <- agg_GA_joined[agg_GA_joined$totalUniqueViews>=5,]

plot8c <- ggplot(agg_GA_joined_overFive, aes(x=scoreOverall, y=totalUniqueViews)) + 
  geom_point() +
  geom_smooth(method="gam") +
  facet_wrap(~year)  +
  xlab("Overall FAIR Score") +
  ylab("Total Unique Views") +
  theme_ADC_modified


#create final plot
plot8c <- plot8c + plot_annotation(
  title = 'Relationship between FAIR and web views',
  subtitle = 'for all datasets with more than five unique views for each year',
  caption = 'updated 2020-11-06
  data current on 2020-10-12 (FAIR) and 2020-10-15 (GA)')


# plot8c
# 
# #save plot as PMG to Aurora
# ggsave(filename="Figure-08c_2020-11-06_Google-Analytics-with-FAIR.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)



######################################
# Figure 8d:
######################################

#remove everything with more than 10 views
agg_GA_joined_overTen <- agg_GA_joined[agg_GA_joined$totalUniqueViews>=10,]

plot8d <- ggplot(agg_GA_joined_overTen, aes(x=scoreOverall, y=totalUniqueViews)) + 
  geom_point() +
  geom_smooth(method="gam") +
  xlab("Overall FAIR Score") +
  ylab("Total Unique Views") +
  theme_ADC_modified


#create final plot
plot8d <- plot8d + plot_annotation(
  title = 'Relationship between FAIR and web views',
  subtitle = 'for all datasets with more than 10 views across all years',
  caption = 'updated 2020-11-06
  data current on 2020-10-12 (FAIR) and 2020-10-15 (GA)')


# plot8d
# 
# #save plot as JPG to Aurora
# ggsave(filename="Figure-08d_2020-11-06_Google-Analytics-with-FAIR.jpg",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)


