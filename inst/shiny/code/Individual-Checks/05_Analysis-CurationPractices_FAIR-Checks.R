#---
#  title: "FAIR Scores: Investigating Curation Practices"
#  author: "Christopher W. Beltz"
#  date created: "2020-11-05"
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


###################################################################
#### Load Common Graphics
###################################################################

#load common graphical parameters, based on `theme_ADC` from "github.nceas.ucsb.edu/KNB/arctic-data/blob/master/reporting/R/theme_ADC.R"
source(here("Data-Fellowship_FAIR-Metrics", "code", "Graphical", "theme_ADC_modified.R"))



###################################################################
#### Load Data
###################################################################

#load cleaned individual check data from 2020-09-30 created using the code chunk above
checks_individual_ADC <- readRDS(here("Data-Fellowship_FAIR-Metrics", "data", "Individual-Checks", "cleaned", "checks_individual_ADC_2020-09-30.rds"))
indivChecks_clean_withCalcs <- readRDS(here("Data-Fellowship_FAIR-Metrics", "data", "Individual-Checks", "cleaned", "indivChecks_clean_2020-09-30.rds"))
remove_series_id <- readRDS(here("Data-Fellowship_FAIR-Metrics", "data", "Individual-Checks", "cleaned", "remove_series_id_2020-09-30.rds"))



###################################################################
#### Data Wrangling
###################################################################

#Subset Data
preADC_series_id <- indivChecks_clean_withCalcs %>%
  group_by(series_id) %>%
  filter(dateSplit=="INITIAL",
         date_uploaded < parse_datetime("2016-03-21")) %>%
  summarise(n=n())

indivChecks_pre2016 <- indivChecks_clean_withCalcs[which((indivChecks_clean_withCalcs$series_id %in% preADC_series_id$series_id)),]

indivChecks_post2016 <- indivChecks_clean_withCalcs[which(!(indivChecks_clean_withCalcs$series_id %in% preADC_series_id$series_id)),]



###################################################################
#### Figure 5a: Metadata meeting individual FAIR checks since inception of ADC
###################################################################



#summarize pre-ADC/ACADIS data
plotData_5_pre <- indivChecks_pre2016 %>%
  group_by(check_type, check_level, check_name, dateSplit) %>%
  summarise(n=n(),
            mean=mean(check_status_num)) %>%
  filter(dateSplit=="FINAL") %>%
  mutate(dateSplit = gsub("FINAL", "ACADIS", dateSplit))


#summarize ADC data
plotData_5_post2016 <- indivChecks_post2016 %>%
  group_by(check_type, check_level, check_name, dateSplit) %>%
  summarise(n=n(),
            mean=mean(check_status_num))

plotData_5_post2016$dateSplit <- factor(plotData_5_post2016$dateSplit, levels = c("INITIAL", "FINAL"))


######################################
# Data Visualization
######################################

#create graphical parameters
colorValues <- c("INITIAL" = "gray60", "FINAL" = "orangered1")
fillValues <- c("INITIAL" = "gray80", "FINAL" = "black", "ACADIS" = "yellow", "2020" = "#19B36A")
shapeValues <- c("INITIAL" = 21, "FINAL" = 21, "ACADIS" = 24, "2020" = 23)
sizeValues <- c("INITIAL" = 2.2, "FINAL" = 2.2, "ACADIS" = 3, "2020" = 3)


plot5a <- ggplot(plotData_5_post2016, aes(x=mean, y=check_name %>% forcats::fct_reorder(-mean))) +
  geom_point(data=plotData_5_post2016, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  geom_line(data=plotData_5_post2016, aes(group=check_name, color=dateSplit), size=1.2) +
  geom_point(data=plotData_5_post2016, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  facet_wrap(~ check_type, scale="free") +
  scale_shape_manual(values=shapeValues,
                     name="Score at",
                     labels=c("initial submission", "final publication")) +
  scale_fill_manual(values=fillValues,
                    name="Score at",
                    labels=c("initial submission", "final publication")) +
  scale_color_manual(values=colorValues,
                     name="Score is",
                     labels=c("improving", "deteriorating")) +
  scale_size_manual(values=sizeValues) +
  theme_ADC_modified +
  xlab("") +
  ylab("") +
  theme(strip.text.x = element_text(size = 14, colour = "black", face="bold"),
        panel.border = element_rect(colour = "black", fill=NA)) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)) +
  guides(size = FALSE,
         fill = guide_legend(override.aes = list(size = 3))
         )


plot5a <- plot5a

# plot_annotation(
#   title = 'Proportion of metadata passing individual FAIR checks',
#   subtitle = 'for the Arctic Data Center repository for, 2016-03-21 to present',
#   caption = 'updated 2020-11-09
#   data current on 2020-09-30'
# )


# plot5a
#
#
# #save plot as PNG to aurora
# ggsave(filename="Figure-05a_2020-11-09_FAIR-IndividualChecksOverTime_post2016.png",
#        path=here("figures"),
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)



###################################################################
#### Figure 5b: Metadata meeting FAIR checks, including ACADIS
###################################################################

plot5b <- ggplot(plotData_5_post2016, aes(x=mean, y=check_name %>% forcats::fct_reorder(-mean))) +
  geom_point(data=plotData_5_post2016, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  geom_line(data=plotData_5_post2016, aes(group=check_name, color=dateSplit), size=1.2) +
  geom_point(data=plotData_5_pre, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  geom_point(data=plotData_5_post2016, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  facet_wrap(~ check_type, scale="free") +
  scale_shape_manual(values=shapeValues,
                     name="Score at",
                     labels=c("pre-ADC", "final publication", "initial submission")) +
  scale_fill_manual(values=fillValues,
                    name="Score at",
                    labels=c("pre-ADC", "final publication", "initial submission")) +
  scale_color_manual(values=colorValues,
                     name="Score is",
                     labels=c("improving", "deteriorating")) +
  scale_size_manual(values=sizeValues) +
  theme_ADC_modified +
  xlab("") +
  ylab("") +
  theme(strip.text.x = element_text(size = 14, colour = "black", face="bold"),
        panel.border = element_rect(colour = "black", fill=NA)) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)) +
  guides(size = FALSE,
         fill = guide_legend(override.aes = list(size = 3))
  )


plot5b <- plot5b

# plot_annotation(
#   title = 'Proportion of metadata passing individual FAIR checks',
#   subtitle = 'for the Arctic Data Center repository and inherited pre-ADC data',
#   caption = 'updated 2020-11-09
#   data current on 2020-09-30'
# )


# plot5b
#
#
# #save plot as PNG to aurora
# ggsave(filename="Figure-05b_2020-11-09_FAIR-IndividualChecksOverTime_withACADIS.png",
#        path=here("figures"),
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)



###################################################################
#### Figure X: Basic Calcs on Length of Submission
###################################################################

all_checks_ADC_post2016 <- checks_individual_ADC[which(!(checks_individual_ADC$series_id %in% preADC_series_id$series_id)),]


test.tmp <- all_checks_ADC_post2016 %>%
  mutate(check_status_num = case_when(
    check_status == "SUCCESS" ~ 1,
    check_status == "FAILURE" ~ 0)) %>%
  arrange(series_id, check_level, check_name, date_uploaded, filename) %>%
  group_by(series_id, check_level, check_name) %>%
  filter(check_status=="SUCCESS") %>%
  mutate(days_since_submission= as.numeric(difftime(date_uploaded, min(date_uploaded), units="days")))

#quick checks
#hist(test.tmp$days_since_submission, breaks=100)
# mean(test.tmp$days_since_submission)
# median(test.tmp$days_since_submission)

test.tmp2 <- test.tmp %>%
  group_by(series_id) %>%
  summarize(submission_length_max = max(days_since_submission),
            n=n()/51)

mean(test.tmp2$submission_length_max)
median(test.tmp2$submission_length_max)

mean(test.tmp2$n)
median(test.tmp2$n)

quantile(test.tmp2$submission_length_max, .95)







