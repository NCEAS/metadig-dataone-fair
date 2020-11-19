#---
#  title: "FAIR Scores: Initial Analysis of Individual Checks"
#  author: "Christopher W. Beltz"
#  date created: "2020-11-04"
#  date updated: "2020-11-04"
#  packages updated: "2020-11-04"
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

# #THIS TAKES ~11 MINUTES TO RUN ON AURORA. FOR CONVENIENCE USE THE ARCHIVED DATA IN THE NEXT CODE CHUCK STARTING ON LINE 42.
# #Load and clean individual check data
# source(here("code", "01a_Cleaning_Individual-FAIR-Checks.R"), local = knitr::knit_global()) #This takes ~30 seconds on Aurora.
# 
# #Subset cleaned data to include only original first/last versions (i.e., metadata submission and final publication), calculate change in FAIR checks over time, and calculated length of curation time (i.e., submission length).
# source(here("code", "02a_Calculation_Individual-FAIR-Checks.R"), local = knitr::knit_global()) #This takes ~10 minutes on Aurora


#load cleaned individual check data from 2020-09-30 created using the code chunk above
checks_individual_ADC <- readRDS(here("data", "Individual-Checks", "cleaned", "checks_individual_ADC_2020-09-30.rds"))
indivChecks_clean_withCalcs <- readRDS(here("data", "Individual-Checks", "cleaned", "indivChecks_clean_2020-09-30.rds"))
remove_series_id <- readRDS(here("data", "Individual-Checks", "cleaned", "remove_series_id_2020-09-30.rds"))



###################################################################
#### Figure 1:
###################################################################

#calculate mean proportion of datasets passing each check for all data
plotData_1_all <- indivChecks_clean_withCalcs %>%
  group_by(check_type, check_level, check_name, dateSplit) %>%
  summarise(n=n(), 
            mean=mean(check_status_num))

#calculate mean proportion of datasets passing checks for data finalized in 2020
plotData_1_2020 <- indivChecks_clean_withCalcs %>%
  filter(dateSplit=="FINAL", year==2020) %>%
  group_by(year, check_type, check_level,check_id, check_name, dateSplit) %>%
  summarise(n=n(), 
            mean=mean(check_status_num))

#change sequence split to "2020" for final year data
plotData_1_2020$dateSplit <- gsub("FINAL", "2020", plotData_1_2020$dateSplit)



######################################
# Data Visualization
######################################

#create graphical parameters
colorValues <- c("INITIAL" = "gray60", "FINAL" = "orangered1")
fillValues <- c("INITIAL" = "gray80", "FINAL" = "black", "2020" = "#19B36A")
shapeValues <- c("INITIAL" = 21, "FINAL" = 21, "2020" = 23)
sizeValues <- c("INITIAL" = 2, "FINAL" = 2, "2020" = 3)


#plot it!
plot_1 <- ggplot(plotData_1_all, aes(x=mean, y=check_name %>% forcats::fct_reorder(-mean))) +
  geom_point(data=plotData_1_2020, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  geom_line(data=plotData_1_all, aes(group=check_name, color=dateSplit), size=1.2) +
  geom_point(data=plotData_1_2020, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  geom_point(data=plotData_1_all, aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
  facet_wrap(~ check_type, scale="free") +
  scale_shape_manual(values=shapeValues,
                     name="FAIR Score at",
                     labels=c("final publication (2020 only)", "final publication (ADC-wide)", "initial submission (ADC-wide)")) +
  scale_fill_manual(values=fillValues,
                    name="FAIR Score at",
                    labels=c("final publication (2020 only)", "final publication (ADC-wide)", "initial submission (ADC-wide)")) +
  scale_color_manual(values=colorValues,
                     name="FAIR Score is",
                     labels=c("improving", "deteriorating")) +
  scale_size_manual(values=sizeValues) +
  theme_ADC_modified +
  xlab("") +
  ylab("") +
  theme(strip.text.x = element_text(size = 14, colour = "black", face="bold"),
        panel.border = element_rect(colour = "black", fill=NA)) +
  theme(legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  guides(size = FALSE, 
         fill = guide_legend(override.aes = list(size = 4)))


plot1 <- plot_1 + plot_annotation(
  title = 'Proportion of metadata meeting individual FAIR checks',
  subtitle = 'for the Arctic Data Center',
  caption = '2020-09-21')


# plot1
# 
# 
# #save plot as PNG to aurora
# ggsave(filename="Figure-01_2020-09-30_FAIR-IndividualChecksOverTime.jpg",
#        path=here("figures"),
#        dpi = 400,
#        units="in",
#        width=13,
#        height=7.3)



###################################################################
#### Figure 2: 
###################################################################

#Figure 2a: Histogram with removed categories highlighted

#create dataset to plot at the individual document (i.e., 'filename' level)
plotData_2a <- checks_individual_ADC%>%
  mutate(removeSplit = case_when(
    is.na(checks_individual_ADC$series_id) == "TRUE" ~ "NO SERIES ID",
    checks_individual_ADC$series_id %in% remove_series_id == "TRUE" ~ "SINGLE PID IN SERIES_ID",
    checks_individual_ADC$check_status== "ERROR" ~ "CHECK_STATUS ERROR",
    TRUE ~ "RETAINED")) %>%
  group_by(series_id, filename) %>%
  summarise(date_uploaded=unique(parse_datetime(date_uploaded)), removeSplit=unique(removeSplit))

#set levels for 'removeSplit'
plotData_2a$removeSplit <- factor(plotData_2a$removeSplit, levels = c("NO SERIES ID", "SINGLE PID IN SERIES_ID", "CHECK_STATUS ERROR", "RETAINED"))


######################################
# Data Visualization
######################################

#create graphical parameters
fillValues <- c("RETAINED" = "gray80", "CHECK_STATUS ERROR" = "dodgerblue" , "NO SERIES ID" = "#146660", "SINGLE PID IN SERIES_ID" = "#19B36A")


#plot it
plot2a_hist <- ggplot(plotData_2a, aes(x=date_uploaded, fill=removeSplit, order=removeSplit)) +
  geom_histogram(bins=50) +
  scale_fill_manual(values=fillValues,
                    name="Status",
                    labels=c("REMOVED: NO SERIES ID", "REMOVED: SINGLE PID IN SERIES_ID", "REMOVED: CHECK_STATUS ERROR", "RETAINED")) +
  theme_ADC_modified +
  xlab("Date of PID upload") +
  ylab("EML docs uploaded (by PID)") +
  theme(legend.position = c(0.60, 0.81),
        legend.direction='vertical',
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"))



###Figure 2b: Histogram with removed categories highlighted

#create dataset to plot at the series_id level
plotData_2b <- checks_individual_ADC%>%
  mutate(removeSplit = case_when(
    is.na(checks_individual_ADC$series_id) == "TRUE" ~ "NO SERIES ID",
    checks_individual_ADC$series_id %in% remove_series_id == "TRUE" ~ "SINGLE PID IN SERIES_ID",
    checks_individual_ADC$check_status== "ERROR" ~ "CHECK_STATUS ERROR",
    TRUE ~ "RETAINED")) %>%
  group_by(removeSplit) %>%
  summarise(count_series_id=length(unique(series_id)))

#set levels for 'removeSplit'
plotData_2b$removeSplit <- factor(plotData_2b$removeSplit, levels = c("NO SERIES ID", "CHECK_STATUS ERROR", "SINGLE PID IN SERIES_ID", "RETAINED"))


######################################
# Data Visualization
######################################

#plot it!
plot2b_hist <- ggplot(plotData_2b, aes(x=removeSplit, y=count_series_id, fill=removeSplit)) +
  geom_col() +
  geom_text(aes(label = count_series_id), position = position_dodge(0.9)) +
  scale_fill_manual(values=fillValues) +
  theme_ADC_modified +
  xlab("") +
  ylab("count of series_id") +
  theme(plot.background = element_rect(fill = "white", color="black")) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = FALSE)



#Combine figures 2a and 2b
plot2_combo <- plot2a_hist + annotation_custom(ggplotGrob(plot2b_hist), 
                                               xmin = parse_datetime("2010-03-01T00:00:00Z"), 
                                               xmax = parse_datetime("2014-01-01T00:00:00Z"), 
                                               ymin = 2000, 
                                               ymax = 4000)


plot2_combo <- plot2_combo + plot_annotation(
  title = 'Data removed from the upcoming FAIR check analysis',
  subtitle = 'using the full corpus for the Arctic Data Center with checks updated on 2020-09-29',
  caption = 'updated 2020-11-06
  date current on 2020-09-30'
  )



# plot2_combo
# 
# #save plot as PNG to aurora
# ggsave(filename="Figure-02_2020-11-06_DataRemoved.jpg",
#        path=here("figures"),
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)



###################################################################
#### Figure 3: 
###################################################################

#create dataset to plot at the check_name/check_id level
plotData_3 <- indivChecks_clean_withCalcs %>%
  group_by(check_type, check_level, check_name) %>%
  summarise(meanDiff = mean(checkDiff),
            scoreSign = sign(meanDiff))

plotData_3 <-indivChecks_clean_withCalcs %>%
  filter(checkDiff==-1) %>%
  group_by(check_type, check_level, check_name) %>%
  summarise(n_series=n()/2) %>%
  right_join(select(plotData_3, "check_name", "meanDiff")) %>%
  mutate(n_series = replace_na(n_series, 0))


######################################
# Data Visualization
######################################

#plot it!
plot3 <- ggplot(plotData_3, aes(x=meanDiff, y=check_name %>% forcats::fct_reorder(-meanDiff), size=n_series)) +
  geom_point() +
  facet_wrap(~ check_type, scale="free") +
  scale_size_area(max_size = 6) +
  theme_ADC_modified +
  coord_cartesian(xlim = c(-0.60, 0.60)) +
  xlab("") +
  ylab("") +
  theme(strip.text.x = element_text(size = 14, colour = "black", face="bold"),
        panel.border = element_rect(colour = "black", fill=NA)) +
  theme(legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  guides(size=guide_legend(title="Series_IDs with Negative Check"))


plot3 <- plot3 + plot_annotation(
  title = 'Mean Change in Proportion of Passing Metadata Checks from Initial to Final Version',
  subtitle = 'calculated using individual FAIR checks for each unique series_ids in the ADC',
  caption = '2020-09-30')



# plot3
# 
# #save plot as JPG to aurora
# ggsave(filename="Figure-03_2020-09-30_CalculatedChangeOfIndividualDocs.jpg",
#        path=here("figures"),
#        dpi = 400,
#        units="in",
#        width=13,
#        height=7.3)



###################################################################
#### Figure 4: Submission date of negative scores for Check: Entity Type Present
###################################################################

#subset data to include only initial versions of datasets with deteriorating check
plot4_data <- indivChecks_clean_withCalcs %>%
  filter(dateSplit=="INITIAL", scoreSign==-1) %>%
  filter(check_name=="Entity Type Present") %>%
  group_by(series_id, filename) %>%
  summarise(date_uploaded=unique(parse_datetime(date_uploaded)))

#plot it!
  plot4 <- ggplot(plot4_data, aes(x=date_uploaded)) +
  geom_histogram(bins=50) +
  theme_ADC_modified +
  xlab("Date of initial submission") +
  ylab("EML docs uploaded (by PID)") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"))

  
plot4 <- plot4 + plot_annotation(
  title = 'Number of submissions for all deteriorating checks for "Entity Type Present"',
  subtitle = 'across the entire ADC corpus',
  caption = '2020-09-28'
)



# plot4
# 
# #save plot as PNG to aurora
# ggsave(filename="Figure-04_2020-09-28_SubmissionDateEntityTypePresent.jpg",
#        path=here("figures"),
#        dpi = 400,
#        units="in",
#        width=13,
#        height=7.3)


