#---
#  title: "FAIR Scores: Aggregate FAIR Scores Analysis for NSF Figure (Take 2)"
#  author: "Christopher W. Beltz"
#  date created: "2020-11-04"
#  date updated: "2020-11-04"
#  packages updated: "2020-11-04"
#  R version: "3.6.3"
#  input: "cleaned CSV of aggregate FAIR scores for ADC-only data"
#  output: "cleaned CSV that contains identified first/last submissions, calculations of change in FAIR scores, and length of submission process"

#---



###################################################################
####  Setup Environment
###################################################################

#load packages
library(dataone)
library(EML)
library(emld)
library(tidyverse)
library(reshape2)
library(patchwork)
library(here)


#Add DataONE token
#options(dataone_token =)


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
#### Data Wrangling
###################################################################

#create list of sequenceIDs for all datasets that were submitted or had a version submitted prior to ADC opening (2016-03-21)
preADC_seqId <- aggChecks_clean_withCalcs %>%
  group_by(sequenceId) %>%
  filter(dateSplit=="INITIAL",
         dateUploaded < parse_datetime("2016-03-21")) %>%
  summarise(n=n(),
            dateSplit = dateSplit,
            dateUploaded = dateUploaded)


#filter out datasets from before 2016-03-21 and use the dateUpload from initial metadata as submissionDate
aggChecks_data_post2016 <- aggChecks_clean_withCalcs %>%
  filter(!sequenceId %in% preADC_seqId$sequenceId) %>%
  group_by(sequenceId) %>%
  summarize(dateSplit=dateSplit,
            submissionDate=dateUploaded[which(dateSplit=="INITIAL")],
            scoreOverall=scoreOverall,
            scoreFindable=scoreFindable,
            scoreAccessible=scoreAccessible,
            scoreInteroperable=scoreInteroperable,
            scoreReusable=scoreReusable)


###########################################################################################
#### Removing Large Data Rescue Package (POTENTIALLY REMOVE THIS CODE IN THE FUTURE - TBD)
###########################################################################################

#POTENTIALLY TO BE DELETED
#removing ~300 datasets from a bunch of glacier photo datasets that were uploaded by the Data Team as part of a data recovery project

print("WARNING: ADD DATAONE TOKEN")

cn <- CNode("PROD")
adc <- getMNode(cn, "urn:node:ARCTIC")
cd <- query(adc, list(q = 'formatType:METADATA AND title:photogrammetric',
                      fl = 'id,dateUploaded,obsoletes',
                      sort = 'dateUploaded+desc',
                      rows='1000000'),
            as = "data.frame")
result <- cd %>%
  filter(is.na(obsoletes))

temp_index <- merge(result, checks_aggregate_ADC, by.x="id", by.y="pid")

aggChecks_data_post2016_noDataTeam <- aggChecks_data_post2016 %>%
  filter(!sequenceId %in% temp_index$sequenceId)



###########################################################################################
#### FIGURE 11: Aggregate FAIR scores timeseries with a static 1-month bin
###########################################################################################


######################################
# Data wrangling a static 1-month bin
######################################

#calculate monthly mean scores and create a "floor date" for each month
data_plot11 <- aggChecks_data_post2016_noDataTeam %>%
  mutate(date_floor = lubridate::floor_date(submissionDate, "1 month")) %>%
  group_by(dateSplit, date_floor) %>%
  summarize(meanOverall=mean(scoreOverall),
            meanFindable=mean(scoreFindable),
            meanAccessible=mean(scoreAccessible),
            meanInteroperable=mean(scoreInteroperable),
            meanReusable=mean(scoreReusable),
            n=n())

#convert data to long format
data_plot11 <- data_plot11 %>%
  pivot_longer(cols = c(meanOverall, meanFindable, meanAccessible, meanInteroperable, meanReusable),
               names_to = "type",
               values_to = "score")

#set levels for better plotting
data_plot11$type <- factor(data_plot11$type, levels = c("meanOverall", "meanFindable", "meanAccessible", "meanInteroperable", "meanReusable", "n"))
data_plot11$dateSplit <- factor(data_plot11$dateSplit, levels = c("INITIAL", "FINAL"))



######################################
# Data Visualization
######################################

#set graphical parameters
colorValues <- c("meanOverall" = "black",
                 "meanFindable" = "darkgreen",
                 "meanAccessible" = "darkblue",
                 "meanInteroperable" = "orange",
                 "meanReusable" = "firebrick")

lineValues <- c("FINAL" = "solid", "INITIAL" = "dotted")


plot11a <- ggplot(data=data_plot11, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot11[which(data_plot11$type=="meanOverall"),], aes(linetype=dateSplit), color="black") +
  ylim(0,1) +
  xlab("Date of Submission to the ADC") +
  ylab("Mean Monthly Score") +
  scale_color_manual(values=colorValues,
                     name="",
                     labels=c("")) +
  scale_linetype_manual(values=lineValues,
                        name="",
                        labels=c("Initial Submission", "Final Publication")) +
  theme_ADC_modified +
  theme(legend.position = "top") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.5) + #metacatUI v1.13.0
  annotate("text", x = as.POSIXct('2017-02-01'), y = 0.95, label = "v1.13.0", color="dodgerblue2") +
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.5) + #metacatUI v1.14.3
  annotate("text", x = as.POSIXct('2017-09-01'), y = 0.45, label = "v1.14.3", color="dodgerblue3") +
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.5) + #metacatUI v2.0
  annotate("text", x = as.POSIXct('2018-03-01'), y = 0.92, label = "metacatUI v2.0", color="dodgerblue") +
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.5) + #metacatUI v2.6.1
  annotate("text", x = as.POSIXct('2019-05-20'), y = 0.98, label = "v2.6.1", color="dodgerblue4") +
  annotate("text",
           x = as.POSIXct('2016-05-15'),
           y = 0.93,
           label = "OVERALL",
           color="black",
           size=7)


plot11b <- ggplot(data=data_plot11, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot11[which(data_plot11$type=="meanFindable"),], aes(linetype=dateSplit, color=type)) +
  ylim(0,1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values=colorValues) +
  scale_linetype_manual(values=lineValues) +
  theme_ADC_modified +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.3) + #metacatUI v1.13.0
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.3) + #metacatUI v1.14.3
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.3) + #metacatUI v2.0
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.3) + #metacatUI v2.6.1
  annotate("text",
           x = as.POSIXct('2016-11-01'),
           y = 0.98,
           label = "FINDABLE",
           color="darkgreen",
           size=3)


plot11c <- ggplot(data=data_plot11, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot11[which(data_plot11$type=="meanAccessible"),], aes(linetype=dateSplit, color=type)) +
  ylim(0,1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values=colorValues) +
  scale_linetype_manual(values=lineValues) +
  theme_ADC_modified +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.3) + #metacatUI v1.13.0
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.3) + #metacatUI v1.14.3
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.3) + #metacatUI v2.0
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.3) + #metacatUI v2.6.1
  annotate("text",
           x = as.POSIXct('2017-01-15'),
           y = 0.98,
           label = "ACCESSIBLE",
           color="darkblue",
           size=3)


plot11d <- ggplot(data=data_plot11, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot11[which(data_plot11$type=="meanInteroperable"),], aes(linetype=dateSplit, color=type)) +
  ylim(0,1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values=colorValues) +
  scale_linetype_manual(values=lineValues) +
  theme_ADC_modified +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.3) + #metacatUI v1.13.0
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.3) + #metacatUI v1.14.3
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.3) + #metacatUI v2.0
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.3) + #metacatUI v2.6.1
  annotate("text",
           x = as.POSIXct('2017-05-15'),
           y = 0.98,
           label = "INTEROPERABLE",
           color="orange",
           size=3)


plot11e <- ggplot(data=data_plot11, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot11[which(data_plot11$type=="meanReusable"),], aes(linetype=dateSplit, color=type)) +
  ylim(0,1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values=colorValues) +
  scale_linetype_manual(values=lineValues) +
  theme_ADC_modified +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.3) + #metacatUI v1.13.0
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.3) + #metacatUI v1.14.3
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.3) + #metacatUI v2.0
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.3) + #metacatUI v2.6.1
  annotate("text",
           x = as.POSIXct('2016-12-01'),
           y = 0.98,
           label = "REUSABLE",
           color="firebrick",
           size=3)


###Create final image using `patchwork`
FAIR_layout <- "
  AAAAAAAA
  AAAAAAAA
  BBCCDDEE
"


#create final plot
plot11 <- plot11a + plot11b + plot11c + plot11d + plot11e +
  plot_layout(design = FAIR_layout)
  # plot_annotation(title = 'Change in FAIR scores from initial submission to final upload (monthly mean)',
  #                 subtitle = '2016-03-21 to present, without Data Team initial uploads',
  #                 caption = 'updated 2020-11-05
  #                 data current on 2020-10-12')

# plot11
#
#
# #save plot as PNG to Aurora
# ggsave(filename="Figure-11_2020-11-05_AggregateScores-FAIR-NSF-MonthlyMean.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=12,
#        height=9)



###########################################################################################
#### FIGURE 12: Aggregate FAIR scores timeseries with a running mean
###########################################################################################

######################################
# Data wrangling a running mean by  month
######################################

#split data into initial and final versions for each dataset
fig12_data_initial <- aggChecks_data_post2016_noDataTeam %>%
  filter(dateSplit=="INITIAL") %>%
  mutate(date_floor = lubridate::floor_date(submissionDate, "1 month")) %>%
  arrange(submissionDate)

fig12_data_final <- aggChecks_data_post2016_noDataTeam %>%
  filter(dateSplit=="FINAL") %>%
  mutate(date_floor = lubridate::floor_date(submissionDate, "1 month")) %>%
  arrange(submissionDate)


#Calculate running mean for each month from ADC openning to current time on x-axis
#TODO_CWB: May need to be turned into a function later.

#initialize data frame for running mean calculations
data_plot12 <- data.frame(
  dateSplit = as.factor(c(rep("INITIAL", 55), rep("FINAL",55))),
  date_floor = as.POSIXct(rep(NA, 110)),
  meanOverall_running = as.numeric(rep(NA, 110)),
  meanFindable_running = as.numeric(rep(NA, 110)),
  meanAccessible_running = as.numeric(rep(NA, 110)),
  meanInteroperable_running = as.numeric(rep(NA, 110)),
  meanReusable_running = as.numeric(rep(NA, 110))
)

#arrange data in order by date
data_plot12$date_floor[which(data_plot12$dateSplit=="INITIAL")] <- sort(unique(fig12_data_initial$date_floor))
data_plot12$date_floor[which(data_plot12$dateSplit=="FINAL")] <- sort(unique(fig12_data_final$date_floor))

#arrange index by date floor
test_index <- sort(unique(fig12_data_final$date_floor))

#calculate running means using `runner`
data_plot12$meanOverall_running[data_plot12$dateSplit=="INITIAL"] <- runner::runner(x=fig12_data_initial$scoreOverall,
                                                                                    f=mean,
                                                                                    idx=fig12_data_initial$date_floor,
                                                                                    at=test_index)

data_plot12$meanFindable_running[data_plot12$dateSplit=="INITIAL"] <- runner::runner(x=fig12_data_initial$scoreFindable,
                                                                                     f=mean,
                                                                                     idx=fig12_data_initial$date_floor,
                                                                                     at=test_index)

data_plot12$meanAccessible_running[data_plot12$dateSplit=="INITIAL"] <- runner::runner(x=fig12_data_initial$scoreAccessible,
                                                                                       f=mean,
                                                                                       idx=fig12_data_initial$date_floor,
                                                                                       at=test_index)

data_plot12$meanInteroperable_running[data_plot12$dateSplit=="INITIAL"] <- runner::runner(x=fig12_data_initial$scoreInteroperable,
                                                                                          f=mean,
                                                                                          idx=fig12_data_initial$date_floor,
                                                                                          at=test_index)

data_plot12$meanReusable_running[data_plot12$dateSplit=="INITIAL"] <- runner::runner(x=fig12_data_initial$scoreReusable,
                                                                                     f=mean,
                                                                                     idx=fig12_data_initial$date_floor,
                                                                                     at=test_index)

data_plot12$meanOverall_running[data_plot12$dateSplit=="FINAL"] <- runner::runner(x=fig12_data_final$scoreOverall,
                                                                                  f=mean,
                                                                                  idx=fig12_data_initial$date_floor,
                                                                                  at=test_index)

data_plot12$meanFindable_running[data_plot12$dateSplit=="FINAL"] <- runner::runner(x=fig12_data_final$scoreFindable,
                                                                                   f=mean,
                                                                                   idx=fig12_data_initial$date_floor,
                                                                                   at=test_index)

data_plot12$meanAccessible_running[data_plot12$dateSplit=="FINAL"] <- runner::runner(x=fig12_data_final$scoreAccessible,
                                                                                     f=mean,
                                                                                     idx=fig12_data_initial$date_floor,
                                                                                     at=test_index)

data_plot12$meanInteroperable_running[data_plot12$dateSplit=="FINAL"] <- runner::runner(x=fig12_data_final$scoreInteroperable,
                                                                                        f=mean,
                                                                                        idx=fig12_data_initial$date_floor,
                                                                                        at=test_index)

data_plot12$meanReusable_running[data_plot12$dateSplit=="FINAL"] <- runner::runner(x=fig12_data_final$scoreReusable,
                                                                                   f=mean,
                                                                                   idx=fig12_data_initial$date_floor,
                                                                                   at=test_index)


#convert to longer format data
data_plot12 <- data_plot12 %>%
  pivot_longer(cols = c(meanOverall_running, meanFindable_running, meanAccessible_running, meanInteroperable_running, meanReusable_running),
               names_to = "type",
               values_to = "score")

#set levels for better plotting order
data_plot12$type <- factor(data_plot12$type, levels = c("meanOverall_running", "meanFindable_running", "meanAccessible_running", "meanInteroperable_running", "meanReusable_running"))
data_plot12$dateSplit <- factor(data_plot12$dateSplit, levels = c("INITIAL", "FINAL"))



######################################
# Data Visualization
######################################

#set graphic parameters
colorValues <- c("meanOverall_running" = "black",
                 "meanFindable_running" = "darkgreen",
                 "meanAccessible_running" = "darkblue",
                 "meanInteroperable_running" = "orange",
                 "meanReusable_running" = "firebrick")

lineValues <- c("FINAL" = "solid", "INITIAL" = "dotted")


#plot 1 of 5 panels
plot12a <- ggplot(data=data_plot12, aes(x=date_floor, y=score)) +
  geom_line(data=data_plot12[which(data_plot12$type=="meanOverall_running"),], aes(linetype=dateSplit), color="black") +
  ylim(0,1) +
  xlab("Date of Submission to the ADC") +
  ylab("Running Mean Monthly Score") +
  scale_color_manual(values=colorValues,
                     name="",
                     labels=c("")) +
  scale_linetype_manual(values=lineValues,
                        name="",
                        labels=c("Initial Submission", "Final Publication")) +
  theme_ADC_modified +
  theme(legend.position = "top") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.5) + #metacatUI v1.13.0
  annotate("text", x = as.POSIXct('2017-02-01'), y = 0.95, label = "v1.13.0", color="dodgerblue2") +
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.5) + #metacatUI v1.14.3
  annotate("text", x = as.POSIXct('2017-09-01'), y = 0.80, label = "v1.14.3", color="dodgerblue3") +
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.5) + #metacatUI v2.0
  annotate("text", x = as.POSIXct('2018-03-01'), y = 0.92, label = "metacatUI v2.0", color="dodgerblue") +
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.5) + #metacatUI v2.6.1
  annotate("text", x = as.POSIXct('2019-05-20'), y = 0.98, label = "v2.6.1", color="dodgerblue4") +
  annotate("text",
           x = as.POSIXct('2016-05-15'),
           y = 0.93,
           label = "OVERALL",
           color="black",
           size=7)


#plot 2 of 5 panels
plot12b <- ggplot(data=data_plot12, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot12[which(data_plot12$type=="meanFindable_running"),], aes(linetype=dateSplit, color=type)) +
  ylim(0,1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values=colorValues) +
  scale_linetype_manual(values=lineValues) +
  theme_ADC_modified +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.3) + #metacatUI v1.13.0
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.3) + #metacatUI v1.14.3
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.3) + #metacatUI v2.0
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.3) + #metacatUI v2.6.1
  annotate("text",
           x = as.POSIXct('2016-11-01'),
           y = 0.98,
           label = "FINDABLE",
           color="darkgreen",
           size=3)


#plot 3 of 5 panels
plot12c <- ggplot(data=data_plot12, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot12[which(data_plot12$type=="meanAccessible_running"),], aes(linetype=dateSplit, color=type)) +
  ylim(0,1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values=colorValues) +
  scale_linetype_manual(values=lineValues) +
  theme_ADC_modified +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.3) + #metacatUI v1.13.0
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.3) + #metacatUI v1.14.3
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.3) + #metacatUI v2.0
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.3) + #metacatUI v2.6.1
  annotate("text",
           x = as.POSIXct('2017-01-15'),
           y = 0.98,
           label = "ACCESSIBLE",
           color="darkblue",
           size=3)


#plot 4 of 5 panels
plot12d <- ggplot(data=data_plot12, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot12[which(data_plot12$type=="meanInteroperable_running"),], aes(linetype=dateSplit, color=type)) +
  ylim(0,1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values=colorValues) +
  scale_linetype_manual(values=lineValues) +
  theme_ADC_modified +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.3) + #metacatUI v1.13.0
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.3) + #metacatUI v1.14.3
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.3) + #metacatUI v2.0
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.3) + #metacatUI v2.6.1
  annotate("text",
           x = as.POSIXct('2017-05-15'),
           y = 0.98,
           label = "INTEROPERABLE",
           color="orange",
           size=3)


#plot 5 of 5 panels
plot12e <- ggplot(data=data_plot12, aes(x=date_floor, y=score, group=interaction(type, dateSplit))) +
  geom_line(data=data_plot12[which(data_plot12$type=="meanReusable_running"),], aes(linetype=dateSplit, color=type)) +
  ylim(0,1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values=colorValues) +
  scale_linetype_manual(values=lineValues) +
  theme_ADC_modified +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.POSIXct("2016-11-17"), linetype="longdash",
             color = "dodgerblue2", size=0.3) + #metacatUI v1.13.0
  geom_vline(xintercept = as.POSIXct("2017-06-23"), linetype="longdash",
             color = "dodgerblue3", size=0.3) + #metacatUI v1.14.3
  geom_vline(xintercept = as.POSIXct("2018-07-09"), linetype="longdash",
             color = "dodgerblue", size=0.3) + #metacatUI v2.0
  geom_vline(xintercept = as.POSIXct("2019-03-20"), linetype="longdash",
             color = "dodgerblue4", size=0.3) + #metacatUI v2.6.1
  annotate("text",
           x = as.POSIXct('2016-12-01'),
           y = 0.98,
           label = "REUSABLE",
           color="firebrick",
           size=3)


###Create final image using `patchwork`
FAIR_layout <- "
  AAAAAAAA
  AAAAAAAA
  BBCCDDEE
"


#create final plot
plot12 <- plot12a + plot12b + plot12c + plot12d + plot12e +
  plot_layout(design = FAIR_layout)
  # plot_annotation(title = 'Change in FAIR scores from initial submission to final upload (running mean)',
  #                 subtitle = '2016-03-21 to present, without Data Team initial uploads',
  #                 caption = 'updated 2020-11-05
  #                 data current on 2020-10-12')

# plot12
#
#
#
# #save plot as PNG to Aurora
# ggsave(filename="Figure-12_2020-11-05_AggregateScores-FAIR-RunningMean.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)


