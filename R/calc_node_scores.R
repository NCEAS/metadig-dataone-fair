#' Fetch Aggregated FAIR Assessment Scores
#'
#' Connects to the DataONE Quality API to fetch raw quality assessment scores
#' for suite: "FAIR-suite-0.5.0"
#'
#' @return A data frame containing the aggregated metrics
#'
#' @importFrom httr GET add_headers content
#' @export
get_agg_data <- function(){
    response <- httr::GET(
        url = "http://api.dataone.org/quality/scores/",
        query = list(
            id = "urn:node:CN",
            suite = "FAIR-suite-0.5.0"
        ),
        httr::add_headers(Accept = "text/csv")
    )
    
    # To view the content as a data frame
    suppressMessages(data <- content(response, as = "parsed", type = "text/csv"))
    return(data)
}

#' Calculate Lifetime FAIR Scores across Nodes
#'
#' Groups historical assessment data by node (`datasource`) and returns lifetime
#' mean averages for all four core FAIR metrics. It filters out obsoleted object 
#' versions and omits testing nodes.
#'
#' @param data A data frame containing raw quality scores from `get_agg_data`.
#'
#' @return A long-format data frame ready for flower pots, with variables \code{repo}, 
#'   \code{label} (the FAIR metric), \code{score}, and \code{score_100} (scaled percentage).
#'
#' @importFrom dplyr filter group_by summarise mutate %>%
#' @importFrom tidyr pivot_longer
#' @export
get_node_scores_all_time <- function(data){
    
    cum_repo <- data %>% 
        filter(is.na(obsoletedBy)) %>% 
        group_by(datasource) %>% 
        summarise(Findable = mean(scoreFindable),
                  Accessible = mean(scoreAccessible),
                  Interoperable = mean(scoreInteroperable),
                  Reusable = mean(scoreReusable)) %>% 
        pivot_longer(cols = c("Findable", "Accessible", "Interoperable", "Reusable"), names_to = 'label', values_to = 'score') %>% 
        mutate(score_100 = score*100) %>% 
        mutate(repo = gsub("urn:node:","" ,datasource)) %>% 
        filter(!grepl("test", tolower(repo)))
    
    return(cum_repo)
    
}

#' Calculate Monthly Mean FAIR Scores
#'
#' Aggregates assessment data into a chronological monthly timeline per node, and computes 
#' means.
#'
#' @param data A data frame containing raw quality scores from `get_agg_data`.
#'
#' @return A long-format data frame showing monthly means.
#'
#' @importFrom lubridate year month
#' @importFrom dplyr mutate filter arrange group_by summarise ungroup select %>%
#' @importFrom tidyr pivot_longer
#' @export
get_scores_mean_ym <- function(data){
    
    cum_repo <- data %>% 
        mutate(ym = as.Date(sprintf("%4s-%02d-01",
                                    lubridate::year(dateUploaded), 
                                    lubridate::month(dateUploaded)))) %>% 
        filter(dateUploaded > as.Date("2000-01-01")) %>% 
        arrange(ym) %>%
        group_by(ym) %>%
        summarise(num_mean = n(),
                  f=mean(scoreFindable),
                  a=mean(scoreAccessible),
                  i=mean(scoreInteroperable),
                  r=mean(scoreReusable), .groups = "keep") %>%
        
        ungroup() %>% 
        select(ym, f, a, i, r, num_mean) %>% 
        pivot_longer(cols = c(f, a, i, r), names_to = "metric", values_to = "score") %>% 
        mutate(score_100 = score*100)
    
    return(cum_repo)
    
}

get_scores_rolling_mean_ym <- function(data){
    
    scores <- data %>% 
        filter(dateUploaded > as.Date("2000-01-01")) %>% 
        mutate(ym = as.Date(sprintf("%4s-%02d-01", year(dateUploaded), month(dateUploaded)))) %>%
        mutate(scoreF = scoreFindable * 100.0) %>%
        mutate(scoreA = scoreAccessible * 100.0) %>%
        mutate(scoreI = scoreInteroperable * 100.0) %>%
        mutate(scoreR = scoreReusable * 100.0) %>% 
        mutate(sequenceId = if_else(is.na(sequenceId), pid, sequenceId))
    
    score_cumulative <- scores %>%
        arrange(ym, sequenceId, dateUploaded) %>%
        group_by(ym, sequenceId) %>%
        top_n(1, dateUploaded) %>% 
        arrange(ym) %>%
        group_by(ym) %>% 
        summarise(f=mean(scoreF), a=mean(scoreA), i=mean(scoreI), r=mean(scoreR), num_mean = n(), .groups = "drop") %>%
        mutate(fc=cummean(f), ac=cummean(a), ic=cummean(i), rc=cummean(r), num_cum_mean = n())
    
    score_cumulative <- score_cumulative %>% 
        select(ym, f, a, i, r, fc, ac, ic, rc, num_mean, num_cum_mean) %>% 
        gather(metric, mean, -ym, -num_mean, -num_cum_mean)
    
    score_cumulative_alone <- score_cumulative %>% filter(metric %in% c('fc', 'ac', 'ic', 'rc'))
    
    score_cumulative_alone$metric <- factor(score_cumulative_alone$metric,
                                            levels=c("fc", "ac", "ic", "rc"),
                                            labels=c("Findable", "Accessible", "Interoperable", "Reusable"))
    
    
    
    return(score_cumulative_alone)
}

get_scores_monthly_state <- function(data){
    global_max_ym <- data %>%
        mutate(ym = as.Date(format(dateUploaded, "%Y-%m-01"))) %>%
        pull(ym) %>%
        max(na.rm = TRUE)
    
    long_df <- data %>%
        mutate(ym = as.Date(format(dateUploaded, "%Y-%m-01"))) %>%
        filter(dateUploaded > as.Date("2000-01-01")) %>%
        
        # extract the last dataset uploaded in each month per sequenceId
        arrange(sequenceId, ym, dateUploaded) %>%
        group_by(sequenceId, ym) %>%
        summarise(
            scoreFindable      = last(scoreFindable),
            scoreAccessible    = last(scoreAccessible),
            scoreInteroperable = last(scoreInteroperable),
            scoreReusable      = last(scoreReusable),
            .groups = "drop"
        ) %>%
        # generate the complete monthly timeline grid for every sequenceId
        group_by(sequenceId) %>%
        complete(ym = seq.Date(min(ym, na.rm = TRUE), global_max_ym, by = "month")) %>%
        # roll the scores forward (LOCF) within each sequence cohort
        # na.rm = FALSE ensures leading NAs (months before initial upload) are preserved
        mutate(across(starts_with("score"), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
        ungroup() %>%
        # calculate monthly means, no cumulative mean since each month contains the most recent version
        # of all datasets published that month and prior
        group_by(ym) %>%
        summarise(
            num_mean = n(),
            f = mean(scoreFindable, na.rm = TRUE),
            a = mean(scoreAccessible, na.rm = TRUE),
            i = mean(scoreInteroperable, na.rm = TRUE),
            r = mean(scoreReusable, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        
        # reshaping
        pivot_longer(
            cols = c(f, a, i, r),
            names_to = "metric",
            values_to = "mean"
        ) %>%
        mutate(mean = mean * 100) %>%
        select(ym, num_mean, metric, mean) %>%
        arrange(ym, metric)
    
    return(as.data.frame(long_df))
}

get_scores_quantiles_ym <- function(data) {
    data %>% mutate(ym = as.Date(sprintf("%4s-%02d-01",
                                    lubridate::year(dateUploaded), 
                                    lubridate::month(dateUploaded)))) %>% 
        filter(dateUploaded > as.Date("2000-01-01")) %>% 
        pivot_longer(
            cols = c(scoreFindable, scoreAccessible, scoreInteroperable, scoreReusable), 
            names_to = "metric", 
            values_to = "score"
        ) %>%
        group_by(ym, metric) %>%
        summarise(
            num_records = n(),
            mean_score  = mean(score, na.rm = TRUE) * 100,
            p25         = quantile(score, 0.25, na.rm = TRUE) * 100,
            p75         = quantile(score, 0.75, na.rm = TRUE) * 100,
            .groups     = "drop"
        )
}
