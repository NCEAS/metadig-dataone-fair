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

#' Calculate Cumulative Monthly Mean FAIR Scores
#'
#' Aggregates assessment data into a chronological monthly timeline per node, and computes 
#' expanding cumulative means tracking scores over time.
#'
#' @param data A data frame containing raw quality scores from `get_agg_data`.
#'
#' @return A long-format data frame showing monthly progression records mapped with their
#'   cumulative scores.
#'
#' @importFrom lubridate year month
#' @importFrom dplyr mutate filter arrange group_by summarise ungroup select %>%
#' @importFrom tidyr pivot_longer
#' @export
get_node_scores_mean_ym <- function(data){
    
    cum_repo <- data %>% 
        mutate(ym = as.Date(sprintf("%4s-%02d-01",
                                    lubridate::year(dateUploaded), 
                                    lubridate::month(dateUploaded)))) %>% 
        filter(dateUploaded > as.Date("2000-01-01")) %>% 
        arrange(datasource, ym) %>%
        group_by(datasource, ym) %>%
        summarise(f=mean(scoreFindable),
                  a=mean(scoreAccessible),
                  i=mean(scoreInteroperable),
                  r=mean(scoreReusable), .groups = "keep") %>%
        mutate(fc=cummean(f), ac=cummean(a), ic=cummean(i), rc=cummean(r)) %>%
        ungroup() %>% 
        select(datasource, ym, f, a, i, r, fc, ac, ic, rc) %>% 
        pivot_longer(cols = c(f, a, i, r, fc, ac, ic, rc), names_to = "metric", values_to = "score") %>% 
        mutate(score_100 = score*100) %>% 
        mutate(repo = gsub("urn:node:","" ,datasource)) %>% 
        filter(!grepl("test", tolower(repo))) 
    
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
