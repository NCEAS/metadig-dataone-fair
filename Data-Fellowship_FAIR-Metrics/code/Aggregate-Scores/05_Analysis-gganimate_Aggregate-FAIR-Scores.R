#---
#  title: "FAIR Scores: Aggregate FAIR Scores Analysis using `gganimate` for NSF Figure"
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
library(tidyverse)
library(reshape2)
library(gganimate)
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

#summarize FAIR scores and uploads on both a weekly and monthly basis
gganimate_NSF_weekly <- checks_aggregate_ADC %>%
  filter(dateUploaded > as.Date("2016-03-20")) %>%
  mutate(year = lubridate::year(dateUploaded),
         month = lubridate::month(dateUploaded),
         week = lubridate::week(dateUploaded),
         date_floor = lubridate::floor_date(dateUploaded, "1 week")) %>%
  group_by(year, month, week, date_floor) %>%
  summarize(n=n(),
            meanOverall = mean(scoreOverall),
            meanFindable = mean(scoreFindable),
            meanAccessible = mean(scoreAccessible),
            meanInteroperable = mean(scoreInteroperable),
            meanReusable = mean(scoreReusable))

#monthly
gganimate_NSF_monthly <- checks_aggregate_ADC %>%
  filter(dateUploaded > as.Date("2016-03-20")) %>%
  mutate(year = lubridate::year(dateUploaded),
         month = lubridate::month(dateUploaded),
         week = lubridate::week(dateUploaded),
         date_floor = lubridate::floor_date(dateUploaded, "1 month")) %>%
  group_by(year, month, date_floor) %>%
  summarize(n=n(),
            meanOverall = mean(scoreOverall),
            meanFindable = mean(scoreFindable),
            meanAccessible = mean(scoreAccessible),
            meanInteroperable = mean(scoreInteroperable),
            meanReusable = mean(scoreReusable))

gganimate_NSF_monthly_nOnly <- gganimate_NSF_monthly %>% 
  select(date_floor, n)

gganimate_NSF_monthly <- gganimate_NSF_monthly %>% 
  select(!n) %>% 
  pivot_longer(cols = c(meanOverall, meanFindable, meanAccessible, meanInteroperable, meanReusable),
               names_to = "type",
               values_to = "score")

#set levels for better plotting later
gganimate_NSF_monthly$type <- factor(gganimate_NSF_monthly$type, levels = c("meanOverall", "meanFindable", "meanAccessible", "meanInteroperable", "meanReusable"))


###########################################################################################
#### FIGURE 10: gganimate
###########################################################################################

#set graphic parameters
colorValues <- c("meanOverall" = "black",
                 "meanFindable" = "darkgreen", 
                 "meanAccessible" = "darkblue",
                 "meanInteroperable" = "orange",
                 "meanReusable" = "firebrick")

lineValues <- c("meanOverall" = "solid",
                 "meanFindable" = "dashed", 
                 "meanAccessible" = "dashed",
                 "meanInteroperable" = "dashed",
                 "meanReusable" = "dashed")

sizeValues <- c("meanOverall" = 1.5,
                "meanFindable" = 0.5, 
                "meanAccessible" = 0.5,
                "meanInteroperable" = 0.5,
                "meanReusable" = 0.5)

alphaValues <- c("meanOverall" = 1.0,
                "meanFindable" = 0.75, 
                "meanAccessible" = 0.75,
                "meanInteroperable" = 0.75,
                "meanReusable" = 0.75)


#create static figure
plot10_raw <- ggplot() +
  annotate('rect', xmin = as.POSIXct("2016-03-22"), xmax = as.POSIXct("2016-12-31"), ymin = -Inf, ymax = -0.1, fill='#084594', alpha=0.3) +
  annotate('rect', xmin = as.POSIXct('2016-12-31'), xmax = as.POSIXct('2017-09-15'), ymin = -Inf, ymax = -0.1, fill='#2171B5', alpha=0.3) +
  annotate('rect', xmin = as.POSIXct('2017-09-15'), xmax = as.POSIXct('2018-06-15'), ymin = -Inf, ymax = -0.1, fill='#4292C6', alpha=0.3) +
  annotate('rect', xmin = as.POSIXct('2018-06-15'), xmax = as.POSIXct('2019-03-15'), ymin = -Inf, ymax = -0.1, fill="#6BAED6", alpha=0.3) +
  annotate('rect', xmin = as.POSIXct('2019-03-15'), xmax = as.POSIXct('2020-01-01'), ymin = -Inf, ymax = -0.1, fill="grey55", alpha=0.3) +
  annotate('rect', xmin = as.POSIXct('2020-01-01'), xmax = as.POSIXct('2020-10-15'), ymin = -Inf, ymax = -0.1, fill="#6BAED6", alpha=0.3) +
  annotate('text', x = as.POSIXct('2016-08-21'), y = -150, label = "ADC Opens", fontface='bold.italic', size = 4) +
  annotate('text', x = as.POSIXct('2017-05-25'), y = -150, label = "special thing\n #1", fontface = 'bold.italic', size = 4) +
  annotate('text', x = as.POSIXct('2018-02-18'), y = -150, label = "special thing\n #2", fontface = 'bold.italic', size = 4) +
  annotate('text', x = as.POSIXct('2018-10-31'), y = -150, label = "special thing\n #3", fontface = 'bold.italic', size = 4) +
  annotate('text', x = as.POSIXct('2019-08-15'), y = -150, label = "FAIR 0.2.1", fontface = 'bold.italic', size = 4) +
  annotate('text', x = as.POSIXct('2020-05-25'), y = -150, label = "FAIR 0.3.2", fontface = 'bold.italic', size = 4) +
  annotate("curve", x = as.POSIXct("2016-09-15"), xend = as.POSIXct("2016-03-30"), y = 3950, yend = 2750, curvature = 0.5, 
           size = 1.0, arrow = arrow(length = unit(2, "mm")), colour = "firebrick") +
  annotate('text', x = as.POSIXct('2017-04-15'), y = 3950, label = "ACADIS data imported", fontface = 'bold.italic', size = 5) +
  geom_bar(data = gganimate_NSF_monthly_nOnly, aes(x=date_floor, y=n, group=seq_along(date_floor)), fill="gray40", stat = 'identity', alpha=0.50) +
  geom_line(data = gganimate_NSF_monthly, aes(x=date_floor, y=score*4000, linetype=type, color=type, size=type, alpha=type)) +
  scale_color_manual(values=colorValues,
                     name="FAIR Score Category:",
                     labels=c("Overall", "Findable", "Accessible", "Interoperable", "Reusable")) +
  scale_linetype_manual(values=lineValues,
                        name="FAIR Score Category:",
                        labels=c("Overall", "Findable", "Accessible", "Interoperable", "Reusable")) +
  scale_size_manual(values=sizeValues,
                    name="FAIR Score Category:",
                    labels=c("Overall", "Findable", "Accessible", "Interoperable", "Reusable")) +
  scale_alpha_manual(values=alphaValues,
                    name="FAIR Score Category:",
                    labels=c("Overall", "Findable", "Accessible", "Interoperable", "Reusable")) +
  scale_y_continuous(name = 'Monthly Dataset Uploads', 
                     sec.axis = sec_axis(~./4000, name = "Mean Monthly FAIR Score")) +
  labs(x = "Date") +
  scale_x_datetime(date_breaks = "1 year", date_labels="%Y") +
  theme_ADC_modified +
  theme(legend.position = "top") +
  theme(axis.line.y.left = element_line(color = "gray40"),
        axis.ticks.y.left = element_line(color = "gray40"),
        axis.text.y.left = element_text(color="gray40"),
        axis.title.y.left = element_text(color="gray40"))

plot10 <-plot10_raw

# #create final plot
# plot10 <- plot10_raw + plot_annotation(
#   title = 'Mean monthly FAIR scores and dataset uploads',
#   subtitle = 'since the inception of the Arctic Data Center',
#   caption = 'updated 2020-11-05
#   data current on 2020-10-12')
# 
# plot10
#
#
# #save plot as PNG to Aurora
# ggsave(filename="Figure-09_2020-11-05_AggregateScores-FAIR-gganimate-static.png",
#        path=here("figures"),
#        bg="transparent",
#        dpi = 400,
#        units="in",
#        width=10,
#        height=8)


#animate static figure by date
plot10_animate <- plot10_raw +
  transition_reveal(date_floor) +
  view_follow()


#create low resolution GIF
#gif_plot10_small <-animate(plot10_animate, nframes=25)

#create high resolution `gganimate` plot (this takes ~2 minutes)
##use ffmpeg for Aurora
#gif_plot10 <- animate(plot10_animate, height=9, width=12, units='in', res=300, fps=10, end_pause = 15, renderer = ffmpeg_renderer())

##use default for personal computer
gif_plot10 <- animate(plot10_animate, height=9, width=12, units='in', res=300, fps=10, end_pause = 15)


#gif_plot10_small
# gif_plot10

# #save animate (either low or high resolution)
# anim_save(here("figures", "Figure-10_2020-11-05_gganimate-FAIR-Scores.gif"), gif_plot10)


