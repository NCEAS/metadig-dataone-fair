#' Get a summary of all the checks and their xpaths/jq
#'
#' @param suite_path 
#' @param checks_dir 
#'
#' @import xml2
#' @export
#'
get_suite_summary <- function(suite_path, checks_dir){
    check_files <- dir(checks_dir, full.names = TRUE)
    all_checks <- lapply(check_files, read_xml)
    check_ids <- xml2::read_xml(suite_path) %>% 
        xml2::xml_find_all("check/id") %>% 
        xml2::xml_text()
    
    fair_checks <- data.frame()
    
    for (check in all_checks) {
        id <- check %>% xml2::xml_find_first("id") %>% xml2::xml_text()
        check_name <- check %>% xml2::xml_find_first("name") %>% xml2::xml_text()
        desc <- check %>% xml2::xml_find_first("description") %>% xml2::xml_text()
        type <- check %>% xml2::xml_find_first("type") %>% xml2::xml_text()
        level <- check %>% xml2::xml_find_first("level") %>% xml2::xml_text()
        
        if (id %in% check_ids) {
            selectors <- xml2::xml_find_all(check, ".//selector")
            
            for (sel in selectors) {
                sel_name <- xml2::xml_find_first(sel, "name") %>% xml2::xml_text() %>% str_trim()
                
                raw_xpath <- xml2::xml_find_first(sel, "xpath") %>% xml2::xml_text()
                parent_paths <- split_compound_paths(raw_xpath)
                
                sub_node <- xml2::xml_find_first(sel, "subSelector")
                
                if (!is.na(sub_node)) {
                    sub_name  <- xml2::xml_find_first(sub_node, "name") %>% xml2::xml_text() %>% str_trim()
                    raw_sub   <- xml2::xml_find_first(sub_node, "xpath") %>% xml2::xml_text()
                    sub_paths <- split_compound_paths(raw_sub)
                } else {
                    sub_name  <- NA_character_
                    sub_paths <- rep(NA_character_, length(parent_paths))
                }
                
                if (length(sub_paths) < length(parent_paths) && !all(is.na(sub_paths))) {
                    sub_paths <- c(sub_paths, rep(NA_character_, length(parent_paths) - length(sub_paths)))
                }
                
                final_targets <- parent_paths
                final_subs    <- sub_paths
                dialect_tags  <- case_when(
                    str_detect(final_targets, "^/?eml") ~ "EML",
                    TRUE ~ "ISO"
                )
                
                raw_expr <- xml2::xml_find_first(sel, "expression") %>% xml2::xml_text()
                if (!is.na(raw_expr) && raw_expr != "") {
                    clean_expr <- str_remove(raw_expr, fixed("(.[\"@graph\"]? // [.] )[] | ")) %>% str_trim()
                    
                    final_targets <- c(final_targets, clean_expr)
                    final_subs    <- c(final_subs, NA_character_)
                    dialect_tags  <- c(dialect_tags, "SOSO")
                }
                
                for (i in seq_along(final_targets)) {
                    target_path <- final_targets[i]
                    target_sub  <- final_subs[i]
                    dialect_assigned <- dialect_tags[i]
                    
                    if (str_detect(target_path, "^/?resource")) {
                        next
                    }
                    
                    fair_checks <- bind_rows(fair_checks, data.frame(
                        check_id      = id,
                        check_name    = check_name,
                        selector_name = sel_name,
                        sub_name      = sub_name,
                        expression    = target_path, 
                        sub_xpath     = target_sub,  
                        dialect       = dialect_assigned
                    ))
                }
            }
        }
    }
    return(fair_checks)
}

split_compound_paths <- function(xpath_str) {
    if (is.na(xpath_str) || xpath_str == "") return(character(0))
    xpath_str <- stringr::str_replace_all(xpath_str, "\\s+", " ") %>% stringr::str_trim()
    xpath_str <- stringr::str_replace(xpath_str, "^boolean\\(\\s*(.*?)\\s*\\)$", "\\1")
    
    protected_str <- str_replace_all(xpath_str, "\\[(.*?)\\]", function(m) {
        m <- stringr::str_replace_all(m, "\\|", "___PIPE___")
        m <- stringr::str_replace_all(m, "\\s+or\\s+", "___OR___") # Protect 'or' inside predicates
        return(m)
    })
    
    paths <- stringr::str_split(protected_str, "\\s*\\|\\s*|\\s+or\\s+")[[1]]
    
    paths <- stringr::str_replace_all(paths, "___PIPE___", "|") %>%
        stringr::str_replace_all("___OR___", " or ") %>%
        stringr::str_replace_all("\\s+", " ") %>%
        stringr::str_trim()
    
    return(paths)
}

