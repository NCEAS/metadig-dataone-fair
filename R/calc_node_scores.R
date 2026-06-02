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
        mutate(fc=cummean(f), ac=cummean(a), ic=cummean(i), rc=cummean(r)) %>%
        ungroup() %>% 
        select(ym, f, a, i, r, fc, ac, ic, rc, num_mean) %>% 
        pivot_longer(cols = c(f, a, i, r, fc, ac, ic, rc), names_to = "metric", values_to = "score") %>% 
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
    dt <- as.data.table(data)
    
    dt[, ym := as.Date(format(dateUploaded, "%Y-%m-01"))]
    dt <- dt[dateUploaded > as.Date("2000-01-01")]
    
    # slice_max equivalent, get last dataset uploaded in each month
    setorder(dt, sequenceId, ym, dateUploaded)
    prepared <- dt[, .(
        scoreFindable = scoreFindable[.N],
        scoreAccessible = scoreAccessible[.N],
        scoreInteroperable = scoreInteroperable[.N],
        scoreReusable = scoreReusable[.N]
    ), by = .(sequenceId, ym)]
    
    global_max_ym <- max(prepared$ym, na.rm = TRUE)
    # generate full timeline grid
    grid <- prepared[, .(ym = seq.Date(min(ym, na.rm = TRUE), global_max_ym, by = "month")), by = sequenceId]
    
    # set keys for matching
    setkey(prepared, sequenceId, ym)
    setkey(grid, sequenceId, ym)
    
    # 'roll = TRUE' means: if a month exists in the grid but not in prepared, 
    # roll the values from the previous available month forward.
    filled_dt <- prepared[grid, roll = TRUE]
    
    # calc monthly means
    summary_dt <- filled_dt[, .(
        num_mean = .N,
        f = mean(scoreFindable, na.rm = TRUE),
        a = mean(scoreAccessible, na.rm = TRUE),
        i = mean(scoreInteroperable, na.rm = TRUE),
        r = mean(scoreReusable, na.rm = TRUE)
    ), by = .(ym)]
    
    # reshape
    long_dt <- melt(
        summary_dt, 
        id.vars = c("ym", "num_mean"), 
        measure.vars = c("f", "a", "i", "r"),
        variable.name = "metric", 
        value.name = "mean"
    )
    
    long_dt[, mean := mean * 100]
    
    # reorder columns
    setcolorder(long_dt, c("ym", "num_mean", "metric", "mean"))
    setorder(long_dt, ym, metric)
    
    return(as.data.frame(long_dt))
}
