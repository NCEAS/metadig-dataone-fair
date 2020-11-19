#---
#  title: "FAIR Scores: High-level Aggregate FAIR Scores Analysis"
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
source(here("code", "Graphical", "theme_ADC_modified.R"))



###################################################################
#### Load Data
###################################################################

#load cleaned aggregate score data from 2020-10-12 created using the code chunk above
checks_aggregate_ADC <- readRDS(here("data", "Aggregate-Scores", "cleaned", "checks_aggregate_ADC_2020-10-12.rds"))
aggChecks_clean_withCalcs <- readRDS(here("data", "Aggregate-Scores", "cleaned", "aggChecks_clean_2020-10-12.rds"))



###################################################################
#### Data Wrangling
###################################################################

#Get sequenceID for all initial versions prior to openning of ADC
preADC_seqId <- aggChecks_clean_withCalcs %>%
  group_by(sequenceId) %>%
  filter(dateSplit=="INITIAL",
         dateUploaded < parse_datetime("2016-03-21")) %>%
  summarise(n=n())

#separate data into pre- and post-ADC openning
aggChecks_pre2016 <- aggChecks_clean_withCalcs[which((aggChecks_clean_withCalcs$sequenceId %in% preADC_seqId$sequenceId)),]
aggChecks_post2016 <- aggChecks_clean_withCalcs[which(!(aggChecks_clean_withCalcs$sequenceId %in% preADC_seqId$sequenceId)),]

#calculate number of ACADIS datasets
preADC_datasets <- nrow(preADC_seqId)


###########################################################################################
#### FIGURE 7a: 
###########################################################################################


######################################
# Data wrangling mean scores
######################################

#get mean of initial and final scores for post-2016 datasets
plotData_7 <- aggChecks_post2016 %>%
  group_by(dateSplit) %>% 
  summarise(OVERALL = mean(scoreOverall),
            Findable = mean(scoreFindable),
            Accessible = mean(scoreAccessible),
            Interoperable = mean(scoreInteroperable),
            Reusable = mean(scoreReusable)) %>%
  pivot_longer(cols=c(OVERALL, Findable, Accessible, Interoperable, Reusable),
               names_to = "scoreType",
               values_to = "meanScore")

#change dateSplit variable names to re-order in legend
plotData_7$dateSplit <- ifelse(plotData_7$dateSplit=="INITIAL", "2INITIAL", "3FINAL")

#change levels for better plotting
plotData_7$dateSplit <- factor(plotData_7$dateSplit, levels = c("1preADC", "2INITIAL", "3FINAL"))
plotData_7$scoreType <- factor(plotData_7$scoreType, levels = c("Reusable", "Interoperable", "Accessible", "Findable", "OVERALL"))


######################################
# Data Visualization
######################################

#create graphical parameters
colorValues <- c("2INITIAL" = "gray60", "3FINAL" = "orangered1")
fillValues <- c("1preADC" = "yellow", "2INITIAL" = "gray80", "3FINAL" = "black")
shapeValues <- c("1preADC" = 23, "2INITIAL" = 21, "3FINAL" = 21)
sizeValues <- c("1preADC" = 2.5, "2INITIAL" = 2.5, "3FINAL" = 2.5)


#plot it!
plot7a <- ggplot(plotData_7, aes(x=meanScore, y=scoreType)) +
  geom_line(aes(group=scoreType), color="gray60", size=2) +
  geom_point(aes(fill=dateSplit, shape=dateSplit), size=4) +
  scale_shape_manual(values=shapeValues,
                     name="Score at",
                     labels=c("initial submission", "final publication")) +
  scale_fill_manual(values=fillValues,
                    name="Score at",
                    labels=c("initial submission", "final publication")) +
  # scale_color_manual(values=colorValues,
  #                    name="Score is",
  #                    labels=c("improving", "deteriorating")) +
  xlim(0,1) +
  theme_ADC_modified +
  xlab("Mean FAIR Score") +
  ylab("") +
  theme(legend.position="top")


# plot7a <- plot7a + plot_annotation(
#   title = 'Proportion of metadata meeting aggregate FAIR Scores',
#   subtitle = 'for the Arctic Data Center (2016-03-21 to present)',
#   caption = 'updated 2020-11-05
#   data current on 2020-10-12')
# 

# plot7a
# 
# 
# #save plot as PNG to Aurora
# ggsave(filename="Figure-07a_2020-11-05_AggregateScores-FAIR-Simple.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)



###########################################################################################
#### FIGURE 7b:
###########################################################################################


######################################
# Data wrangling
######################################

#summarize the RAW initial and final scores for pre-2016 data
plotData_7_pre2016 <- aggChecks_pre2016 %>%
  group_by(dateSplit) %>% 
  summarise(OVERALL = mean(scoreOverall),
            Findable = mean(scoreFindable),
            Accessible = mean(scoreAccessible),
            Interoperable = mean(scoreInteroperable),
            Reusable = mean(scoreReusable)) %>%
  pivot_longer(cols=c(OVERALL, Findable, Accessible, Interoperable, Reusable),
               names_to = "scoreType",
               values_to = "meanScore") %>%
  filter(dateSplit=="FINAL") %>%
  mutate(dateSplit="1preADC")

#change levels for better plotting
plotData_7_pre2016$scoreType <- factor(plotData_7_pre2016$scoreType, levels = c("Reusable", "Interoperable", "Accessible", "Findable", "OVERALL"))


######################################
# Data Visualization
######################################

#plot it!
plot7b <- ggplot(plotData_7, aes(x=meanScore, y=scoreType)) +
  geom_line(aes(group=scoreType, color=dateSplit), size=1.2) +
  geom_point(aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  geom_point(data = plotData_7_pre2016, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  scale_shape_manual(values=shapeValues,
                     name="FAIR Score for",
                     labels=c( "pre-ADC data", "initial submission", "final publication")) +
  scale_fill_manual(values=fillValues,
                    name="FAIR Score for",
                    labels=c( "pre-ADC data", "initial submission", "final publication")) +
  scale_color_manual(values=colorValues,
                     name="FAIR Score is",
                     labels=c("improving", "deteriorating")) +
  scale_size_manual(values=sizeValues) +
  xlim(0,1) +
  theme_ADC_modified +
  xlab("Mean Score") +
  ylab("FAIR Category") +
  theme(legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  guides(size = FALSE, 
         fill = guide_legend(override.aes = list(size = 3)))


plot7b <- plot7b + plot_annotation(
  title = 'Proportion of metadata meeting aggregate FAIR Scores',
  subtitle = 'for  pre-ADC data (before 2016-03-21) and ADC data (after 2016-03-21)',
  caption = '2020-11-05
  data current on 2020-10-12')



# plot7b
# 
# 
# #save plot as PNG to Aurora
# ggsave(filename="Figure-07b_2020-11-05_AggregateScores-FAIR-Simple.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)


