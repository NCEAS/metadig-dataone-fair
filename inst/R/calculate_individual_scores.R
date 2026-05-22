##WORK IN PROGRESS##

#load packages
library(tidyverse)
library(reshape2)
library(patchwork)
library(here)
library(arrow)


out_checks <- here("../metadig-data/check-data-0.5.0")
out_runs <- here("../metadig-data/run-data-0.5.0")

checks <- open_dataset(out_checks)
runs <- open_dataset(out_runs)

runs_first_last <- runs %>% 
    filter(!(run_status %in% c("ERROR", "failure", "processing"))) %>% 
    mutate(series_id = coalesce(series_id, obj_id)) %>% 
    arrange(date_uploaded) %>% 
    group_by(series_id) %>% 
    collect() %>% 
    mutate(ser_version = row_number()) %>% 
    filter(n() > 1 & (ser_version == 1 | ser_version == n())) %>%
    mutate(version = if_else(ser_version == 1, "INITIAL", "FINAL")) %>% 
    mutate(submissionLength_days = as.numeric(difftime(last(date_uploaded), 
                                                first(date_uploaded), 
                                                units = "days"))) %>% 
    ungroup()

checks_clean <- checks %>% 
    inner_join(runs_first_last) %>% 
    mutate(check_status = if_else(grepl("error", tolower(check_output)), "ERROR", check_status)) %>% 
    filter(check_status != "ERROR") %>% 
    mutate(check_status_num = case_when(
        check_status == "SUCCESS" ~ 1,
        check_status == "FAILURE" ~ 0))






