#---
#  title: "FAIR Scores: Initial Analysis of Aggregate FAIR Scores"
#  author: "Christopher W. Beltz"
#  date created: "2020-11-04"
#  date updated: "2020-11-04"
#  packages updated: "2020-11-04"
#  R version: "3.6.3"
#  input: "cleaned CSV of aggregate FAIR scores for ADC-only data"
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
source(here("Data-Fellowship_FAIR-Metrics", "code", "Graphical", "theme_ADC_modified.R"))



###################################################################
#### Load Data
###################################################################

#load cleaned aggregate score data from 2020-10-12 created using the code chunk above
checks_aggregate_ADC <- readRDS(here("Data-Fellowship_FAIR-Metrics", "data", "Aggregate-Scores", "cleaned", "checks_aggregate_ADC_2020-10-12.rds"))
aggChecks_clean_withCalcs <- readRDS(here("Data-Fellowship_FAIR-Metrics", "data", "Aggregate-Scores", "cleaned", "aggChecks_clean_2020-10-12.rds"))



###################################################################
#### Data Exploration
###################################################################

#quick histogram of scores (Note: not sure what is up with the negative scores)
ggplot(aggChecks_clean_withCalcs, aes(x=overallDIFF)) +
  geom_histogram(color="black", fill="white")



###########################################################################################
#### FIGURE 6a:
###########################################################################################


######################################
# Data wrangling
######################################

#subset data for initial only
agg_INITIAL <- aggChecks_clean_withCalcs %>%
  filter(dateSplit=="INITIAL")



######################################
# Data Visualization
######################################

#over time
plot.overall <- ggplot(agg_INITIAL[agg_INITIAL$dateSplit=="INITIAL",], aes(x=dateUploaded, y=overallDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  ylab("Change in Overall FAIR Score") +
  xlab("Date of Initial Dataset Submission") +
  theme_minimal()

plot.F <- ggplot(agg_INITIAL[agg_INITIAL$dateSplit=="INITIAL",], aes(x=dateUploaded, y=findableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.A <- ggplot(agg_INITIAL[agg_INITIAL$dateSplit=="INITIAL",], aes(x=dateUploaded, y=accessibleDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.I <- ggplot(agg_INITIAL[agg_INITIAL$dateSplit=="INITIAL",], aes(x=dateUploaded, y=interoperableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.R <- ggplot(agg_INITIAL[agg_INITIAL$dateSplit=="INITIAL",], aes(x=dateUploaded, y=reusableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())


####Create final combined image using `patchwork`
#NOTE: https://patchwork.data-imaginist.com/articles/guides/layout.html
FAIR_layout <- "
  AAAAAA
  AAAAAA
  BBBCCC
  BBBCCC
  DDDEEE
  DDDEEE
"


#create final plot
plot6a <- plot.overall + plot.F + plot.A + plot.I + plot.R +
  plot_layout(design = FAIR_layout) +
  plot_annotation(title = 'Change in FAIR scores from initial submission to final upload',
                  caption = '2020-10-09',
                  subtitle = '2010 - present')

# plot6a
#
#
# #save plot as PNG to aurora
# ggsave(filename="Figure-06a_2020-10-09_AggregateScores-FAIR-ChangeOverTime-SubmissionDate.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 300,
#        units="in",
#        width=9.5,
#        height=6)



###########################################################################################
#### FIGURE 6b:
###########################################################################################


######################################
# Data wrangling
######################################

#subset data for FINAL only
agg_FINAL <- aggChecks_clean_withCalcs %>%
  filter(dateSplit=="FINAL")



######################################
# Data Visualization
######################################

#over time
plot.overall <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL",], aes(x=dateUploaded, y=overallDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  ylab("Change in Overall FAIR Score") +
  xlab("Date of Final Dataset Publication") +
  theme_minimal()

plot.F <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL",], aes(x=dateUploaded, y=findableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.A <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL",], aes(x=dateUploaded, y=accessibleDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.I <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL",], aes(x=dateUploaded, y=interoperableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.R <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL",], aes(x=dateUploaded, y=reusableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())


####Create final combined image using `patchwork`
#NOTE: https://patchwork.data-imaginist.com/articles/guides/layout.html
FAIR_layout <- "
  AAAAAA
  AAAAAA
  BBBCCC
  BBBCCC
  DDDEEE
  DDDEEE
"


#create final plot
plot6b <- plot.overall + plot.F + plot.A + plot.I + plot.R +
  plot_layout(design = FAIR_layout) +
  plot_annotation(title = 'Change in FAIR scores from initial submission to final upload',
                  caption = '2020-10-09',
                  subtitle = '2010 - present')

# plot6b
#
#
# #save plot as PNG to aurora
# ggsave(filename="Figure-06b_2020-10-09_AggregateScores-FAIR-ChangeOverTime-PublicationDate.jpg",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 300,
#        units="in",
#        width=9.5,
#        height=6)



###########################################################################################
#### FIGURE 6c:
###########################################################################################


######################################
# Data Visualization
######################################

#over time
plot.overall <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL" & agg_FINAL$dateUploaded>as.POSIXct("2016-03-21 00:00:01"),], aes(x=dateUploaded, y=overallDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  ylab("Change in Overall FAIR Score") +
  xlab("Date of Final Dataset Publication") +
  theme_minimal()

plot.F <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL" & agg_FINAL$dateUploaded>as.POSIXct("2016-03-21 00:00:01"),], aes(x=dateUploaded, y=findableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.A <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL" & agg_FINAL$dateUploaded>as.POSIXct("2016-03-21 00:00:01"),], aes(x=dateUploaded, y=accessibleDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.I <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL" & agg_FINAL$dateUploaded>as.POSIXct("2016-03-21 00:00:01"),], aes(x=dateUploaded, y=interoperableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot.R <- ggplot(agg_FINAL[agg_FINAL$dateSplit=="FINAL" & agg_FINAL$dateUploaded>as.POSIXct("2016-03-21 00:00:01"),], aes(x=dateUploaded, y=reusableDIFF)) +
  geom_point(alpha=0.5, fill="#B5E1E7", color="1D254E", pch=21, size=1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank())


####Create final combined image using `patchwork`
#NOTE: https://patchwork.data-imaginist.com/articles/guides/layout.html
FAIR_layout <- "
  AAAAAA
  AAAAAA
  BBBCCC
  BBBCCC
  DDDEEE
  DDDEEE
"


#create final plot
plot6c <- plot.overall + plot.F + plot.A + plot.I + plot.R +
  plot_layout(design = FAIR_layout) +
  plot_annotation(title = 'Change in FAIR scores from initial submission to final upload',
                  caption = '2020-10-09',
                  subtitle = '2016-03-21 to present')

# plot6c
#
#
# #save plot as PNG to aurora
# ggsave(filename="Figure-06c_2020-10-09_AggregateScores-FAIR-ChangeOverTime-PublicationDate-post2016.jpg",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 300,
#        units="in",
#        width=9.5,
#        height=6)


