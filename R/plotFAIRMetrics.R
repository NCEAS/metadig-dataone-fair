

knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(lubridate)


updates <- data.frame(v1=seq(as.Date("2000/1/1"), by = "month", length.out = 10),
                      v2=seq(as.Date("2000/5/1"), by = "month", length.out = 10),
                      v3=seq(as.Date("2000/8/1"), by = "month", length.out = 10)) %>%
  gather(key = version, value = update_date) %>%
  arrange(update_date) %>% select(update_date)
fair <- expand.grid(version = seq(1,3,1), object = seq(1,5,1), scope = c("adc", "knb")) %>%
  mutate(pid = paste(scope, object, version, sep=".")) %>%
  mutate(f = sample(90:100, 30, replace=TRUE)) %>%
  mutate(a = sample(70:100, 30, replace=TRUE)) %>%
  mutate(i = sample(40:70, 30, replace=TRUE)) %>%
  mutate(r = sample(30:50, 30, replace=TRUE)) %>%
  mutate(score = as.integer((f + a + i + r)/4))
scores <- bind_cols(updates, fair)
head(scores)

## Calculate stats

# Generate stats by first groupinng by month, then keep only the most recent
# observation for each dataset that month, and then take the average of those
# for each of the FAIR facets by month.  Finally, transpose the data.
most_recent <- scores %>%
  arrange(update_date, object, version) %>%
  group_by(update_date, object) %>%
  top_n(1, version)
score_cumulative <- most_recent %>%
  summarize(f=cummean(f), a=cummean(a), i=cummean(i), r=cummean(r)) %>%
  select(-object) %>%
  gather(metric, mean, -update_date)
score_cumulative$metric <- factor(score_cumulative$metric,
                             levels=c("f", "a", "i", "r"),
                             labels=c("Findable", "Accessible", "Interoperable", "Reusable"))

d1_colors <- c("#ff582d", "#c70a61", "#1a6379", "#60c5e4")
ggplot(data=score_cumulative, mapping=aes(x=update_date, y=mean, color=metric)) +
  geom_line() +
  geom_point(size=1) +
  theme_bw() +
  scale_colour_manual(values=d1_colors) +
  scale_x_date(date_breaks="3 months", date_minor_breaks="months", labels=date_format("%Y %b")) +
  scale_y_continuous(limits=c(0,100))

score_means <- most_recent %>%
  group_by(update_date) %>%
  summarize(f=mean(f), a=mean(a), i=mean(i), r=mean(r)) %>%
  gather(metric, mean, -update_date)
score_means$metric <- factor(score_means$metric,
                             levels=c("f", "a", "i", "r"),
                             labels=c("Findable", "Accessible", "Interoperable", "Reusable"))
head(score_means)

## Plot it!

d1_colors <- c("#ff582d", "#c70a61", "#1a6379", "#60c5e4")
ggplot(data=score_means, mapping=aes(x=update_date, y=mean, color=metric)) +
  geom_line() +
  geom_point(size=1) +
  theme_bw() +
  scale_colour_manual(values=d1_colors) +
  scale_x_date(date_breaks="3 months", date_minor_breaks="months", labels=date_format("%Y %b")) +
  scale_y_continuous(limits=c(0,100))


fsr <- read_csv("FAIR-scores-ranked.csv")
scores <- mutate(fsr, ym = as.Date(sprintf("%4s-%02d-01", year(dateUploaded), month(dateUploaded)))) %>%
      mutate(scoreF = scoreFindable * 100.0) %>%
      mutate(scoreA = scoreAccessible * 100.0) %>%
      mutate(scoreI = scoreInteroperable * 100.0) %>%
      mutate(scoreR = scoreReusable * 100.0)

most_recent <- scores %>%
  arrange(ym, seriesId, version) %>%
  group_by(ym, seriesId) %>%
  top_n(1, version)

standards <- data.frame(table(most_recent$formatId)) %>%
  rename(format=Var1, count=Freq) %>%
  mutate(family = case_when(grepl("eml", format) ~ "EML", grepl("iso", format) ~ "ISO")) %>%
  group_by(family) %>%
  summarize(n=sum(count))

score_cumulative <- most_recent %>%
  arrange(ym) %>%
  group_by(ym) %>%
  summarise(f=mean(scoreF), a=mean(scoreA), i=mean(scoreI), r=mean(scoreR)) %>%
  mutate(fc=cummean(f), ac=cummean(a), ic=cummean(i), rc=cummean(r)) %>%
  select(ym, f, a, i, r, fc, ac, ic, rc) %>%
  gather(metric, mean, -ym)
score_cumulative$metric <- factor(score_cumulative$metric,
                                  levels=c("f", "a", "i", "r", "fc", "ac", "ic", "rc"),
                                  labels=c("Findable", "Accessible", "Interoperable", "Reusable",
                                           "Cum. Findable", "Cum. Accessible", "Cum. Interoperable", "Cum. Reusable"))

## Plot it!

d1_colors <- c("#ff582d", "#c70a61", "#1a6379", "#60c5e4", "#ff582d", "#c70a61", "#1a6379", "#60c5e4")
ggplot(data=score_cumulative, mapping=aes(x=ym, y=mean, color=metric)) +
  geom_line() +
  geom_point(size=1) +
  theme_bw() +
  scale_colour_manual(values=d1_colors) +
  scale_x_date(date_breaks="year", date_minor_breaks="months", labels=date_format("%Y")) +
  xlab("Year") +
  scale_y_continuous(limits=c(0,100)) +
  ylab("Average FAIR Score") +
  ggtitle(paste0("FAIR scores for ", format(sum(standards$n), big.mark=","), " EML and ISO records"))
