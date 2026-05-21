get_node_scores <- function(){
    
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
    cum_repo <- data %>% 
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